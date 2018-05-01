package sbt.internal.io

import java.io.IOException
import java.nio.file.StandardWatchEventKinds.OVERFLOW
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._

/**
 * Waits for build triggers. Builds can be triggered by source file updates as well as when the
 * watch is terminated by user input.
 *
 * An EventMonitor can be made using the apply method in the EventMonitor companion. Using this
 * handle, the user can block the current thread with awaitEvents until a build is triggered.
 * SBT has a few keys (watchingMessage, triggeredMessage) and internal tasks that require a
 * WatchState. The EventMonitor provides access to a WatchState via the state() method to ensure
 * that the apis that
 * use these keys continue working.
 *
 * No implementation details are specified so that the EventMonitor may be treated as a black box.
 */
private[sbt] sealed trait EventMonitor extends AutoCloseable {

  /** Block indefinitely until the trigger receives a file event or the user stops the watch. */
  def awaitEvent(): Boolean

  /** A snapshot of the WatchState that includes the number of build triggers and watch sources. */
  def state(): WatchState
}

/**
 * Provides factory methods for creating instances of EventMonitor.
 */
private[sbt] object EventMonitor {
  private sealed trait Event
  private case object Cancelled extends Event
  private case class Triggered(path: Path) extends Event

  private[this] class EventMonitorImpl private[EventMonitor] (
      private[this] val trigger: Trigger,
      private[this] val eventThread: Looper with HasState,
      private[this] val userInputThread: Looper,
      private[this] val logger: Logger,
      private[this] val closeService: Boolean)
      extends AntiEntropyEventMonitor(trigger, eventThread.state(), logger) {
    override def close(): Unit = {
      if (closed.compareAndSet(false, true)) {
        if (closeService) eventThread.state().service.close()
        userInputThread.close()
        eventThread.close()
        logger.debug("Closed EventMonitor")
      }
    }
    override def state(): WatchState = eventThread.state().withCount(count)
    private[this] val closed = new AtomicBoolean(false)
  }

  /**
   * Create a new EventMonitor
   *
   * @param state                The initial watch state for the trigger
   * @param delay                Maximum duration that the trigger will poll the watch service for events
   * @param antiEntropy          Minimum duration that must elapse before a build may by re-triggered by
   *                             the same file
   * @param terminationCondition Exits watch if evaluates to true
   * @param logger               Logs output
   * @return The new EventMonitor
   */
  def apply(state: WatchState,
            delay: FiniteDuration,
            antiEntropy: FiniteDuration,
            terminationCondition: => Boolean,
            logger: Logger = NullLogger): EventMonitor =
    applyImpl(state, delay, antiEntropy, terminationCondition, logger, closeService = true)

  private[EventMonitor] def applyImpl(state: WatchState,
                                      delay: FiniteDuration,
                                      antiEntropy: FiniteDuration,
                                      terminationCondition: => Boolean,
                                      logger: Logger,
                                      closeService: Boolean): EventMonitor = {
    val events = new ArrayBlockingQueue[Event](1)
    val trigger = new Trigger(events, state.accept, antiEntropy, logger)
    val eventThread = newEventsThread(delay, state, trigger, logger)
    val userInputThread = newUserInputThread(terminationCondition, events, logger)
    new EventMonitorImpl(trigger, eventThread, userInputThread, logger, closeService)
  }

  private[io] def legacy(state: WatchState,
                         delay: FiniteDuration,
                         terminationCondition: => Boolean): EventMonitor =
    applyImpl(state, delay, 40.milliseconds, terminationCondition, NullLogger, closeService = false)

  private trait HasState {
    def state(): WatchState
  }
  private def newEventsThread(delay: FiniteDuration,
                              s: WatchState,
                              trigger: Trigger,
                              logger: Logger): Looper with HasState = {
    var registered = s.registered
    val lock = new Object
    new Looper(s"watch-state-event-thread-${eventThreadId.incrementAndGet()}") with HasState {
      override def state(): WatchState = lock.synchronized(s.withRegistered(registered))
      override def loop(): Unit = {
        trigger.updateRecentEvents()
        getFilesForKey(s.service.poll(delay)).foreach(trigger.maybeTrigger)
      }
      def getFilesForKey(key: WatchKey): Vector[Path] = key match {
        case null => Vector.empty
        case k =>
          val rawEvents = k.synchronized {
            val events = k.pollEvents.asScala.toVector
            k.reset()
            events
          }
          val keyPath = k.watchable.asInstanceOf[Path]
          val allEvents = rawEvents.flatMap {
            case e if e.kind.equals(OVERFLOW) =>
              handleOverflow(k)
            case e if !e.kind.equals(OVERFLOW) && e.context != null =>
              Some(keyPath.resolve(e.context.asInstanceOf[Path]))
            case _ => None
          }
          logger.debug(s"Received events:\n${allEvents.mkString("\n")}")
          val (exist, notExist) = allEvents.partition(Files.exists(_))
          val (updatedDirectories, updatedFiles) = exist.partition(Files.isDirectory(_))
          val newFiles = updatedDirectories.flatMap(filesForNewDirectory)
          lock.synchronized { registered --= notExist }
          notExist.foreach(s.unregister)
          updatedFiles ++ newFiles ++ notExist
      }

      /*
       * In the case of an overflow, we must poll the file system to find out if there are added
       * or removed directories. When there are new directories, we also want to return file
       * events for the files that are found therein. Because an overflow is likely to occur while
       * a directory is still being modified, we poll repeatedly until we get the same list of
       * files consecutively. We will not trigger for any files that are updated while the WatchKey
       * is in the OVERFLOW state. There is no good way to fix this without caching mtimes for
       * all of the files, which I don't think is worth doing at this juncture.
       */
      private def handleOverflow(key: WatchKey): Vector[Path] = lock.synchronized {
        val allFiles = new mutable.HashSet[Path]
        def addNewFiles(): Unit = {
          allFiles.clear()
          val path = key.watchable.asInstanceOf[Path]
          Files.walkFileTree(
            path,
            new FileVisitor[Path] {
              override def preVisitDirectory(dir: Path,
                                             attrs: BasicFileAttributes): FileVisitResult = {
                allFiles += dir
                if (!registered.contains(dir)) registered += dir -> s.register(dir)
                FileVisitResult.CONTINUE
              }
              override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
                allFiles += file
                FileVisitResult.CONTINUE
              }
              override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
                FileVisitResult.SKIP_SUBTREE
              override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
                FileVisitResult.CONTINUE
            }
          )
          ()
        }

        var oldFiles = mutable.Set.empty[Path]
        do {
          oldFiles = allFiles
          addNewFiles()
        } while (oldFiles != allFiles)
        registered --= registered.collect {
          case (d, k) if !Files.exists(d) =>
            k.reset()
            k.cancel()
            d
        }
        allFiles.toVector
      }

      /*
       * Returns new files found in new directory and any subdirectories, assuming that there is
       * a recursive source with a base that is parent to the directory.
       */
      private def filesForNewDirectory(dir: Path): Iterator[Path] = {
        lazy val recursive =
          s.sources.exists(src => dir.startsWith(src.base.toPath) && src.recursive)
        if (!registered.contains(dir) && recursive) {
          val dirs = Files.walk(dir).iterator.asScala.filter(Files.isDirectory(_))
          val newDirs = dirs.map(d => d -> s.register(d)).toIndexedSeq
          lock.synchronized { registered ++= newDirs }
          Files.walk(dir).iterator.asScala
        } else Nil.iterator
      }
    }
  }
  // Shutup the compiler about unused arguments
  @inline private[this] def ignoreArg(arg: => Any): Unit = if (true) () else { arg; () }
  trait Logger {
    def debug(msg: => Any): Unit = ignoreArg(msg)
  }
  object NullLogger extends Logger
  private def newUserInputThread(terminationCondition: => Boolean,
                                 events: ArrayBlockingQueue[Event],
                                 logger: Logger): Looper =
    new Looper(s"watch-state-user-input-${userInputId.incrementAndGet}") {
      override final def loop(): Unit = {
        Thread.sleep(10)
        if (terminationCondition) {
          logger.debug("Received termination condition. Stopping watch...")
          events.peek match {
            case Cancelled =>
            case _ =>
              while (!events.offer(Cancelled)) {
                events.clear()
              }
          }
        } else {}
      }
    }

  private[this] abstract class AntiEntropyEventMonitor(trigger: Trigger,
                                                       s: WatchState,
                                                       logger: Logger)
      extends EventMonitor {
    private[this] var _count = s.count
    def count: Int = _count
    override def awaitEvent(): Boolean = trigger.events.take() match {
      case Cancelled => false
      case Triggered(path) =>
        _count += 1
        logger.debug(s"Triggered event for path $path")
        true
    }
  }

  private class Trigger(val events: ArrayBlockingQueue[Event],
                        accept: Path => Boolean,
                        antiEntropy: FiniteDuration,
                        logger: Logger) {
    private[this] var recentEvents: Map[Path, Deadline] = Map.empty[Path, Deadline]
    val maybeTrigger: Path => Unit = path => {
      updateRecentEvents()
      if (accept(path)) {
        if (recentEvents.get(path).fold(false)(!_.isOverdue)) {
          logger.debug(s"Ignoring watch event for $path due to anti-entropy constraint")
        } else
          events.peek() match {
            case Cancelled =>
              logger.debug(s"Watch cancelled, not offering event for path $path")
            case _ =>
              recentEvents = recentEvents + (path -> antiEntropy.fromNow)
              if (!events.offer(Triggered(path))) {
                logger.debug(s"Event already pending, dropping event for path: $path")
              }
          }
      }
    }
    def updateRecentEvents(): Unit = { recentEvents = recentEvents.filterNot(_._2.isOverdue) }
  }

  private abstract class Looper(name: String) extends Thread(name) with AutoCloseable {
    private[this] val stopped = new AtomicBoolean(false)
    private[this] var started = false
    private[this] val lock = new Object()
    def loop(): Unit
    @tailrec
    private final def runImpl(firstTime: Boolean): Unit = {
      if (firstTime) lock.synchronized {
        started = true
        lock.notifyAll()
      }
      try {
        if (!stopped.get) {
          loop()
        }
      } catch {
        case _: ClosedWatchServiceException | _: InterruptedException => stopped.set(true)
      }
      if (!stopped.get) {
        runImpl(firstTime = false)
      }
    }
    override final def run(): Unit = runImpl(firstTime = true)
    def close(): Unit = {
      if (stopped.compareAndSet(false, true)) {
        this.interrupt()
        this.join(5000)
      }
    }
    setDaemon(true)
    start()
    lock.synchronized { if (!started) lock.wait() }
  }
  private val eventThreadId = new AtomicInteger(0)
  private val userInputId = new AtomicInteger(0)

}
