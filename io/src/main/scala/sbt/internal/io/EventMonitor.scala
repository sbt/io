package sbt.internal.io

import java.io.IOException
import java.nio.file.StandardWatchEventKinds.OVERFLOW
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }

import sbt.io.WatchService

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

  /** Block indefinitely until the monitor receives a file event or the user stops the watch. */
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

  private class EventMonitorImpl private[EventMonitor] (
      private[this] val service: WatchService,
      private[this] val events: ArrayBlockingQueue[Event],
      private[this] val eventThread: Looper with HasWatchState,
      private[this] val userInputThread: Looper,
      private[this] val logger: Logger,
      private[this] val closeService: Boolean)
      extends EventMonitor {

    override def state(): WatchState = eventThread.state()

    override def awaitEvent(): Boolean = events.take() match {
      case Cancelled => false
      case Triggered(path) =>
        logger.debug(s"Triggered watch event due to updated path: $path")
        eventThread.incrementCount()
        true
    }

    override def close(): Unit = {
      if (closed.compareAndSet(false, true)) {
        if (closeService) service.close()
        userInputThread.close()
        eventThread.close()
        logger.debug("Closed EventMonitor")
      }
    }

    private[this] val closed = new AtomicBoolean(false)
  }

  /**
   * Create a new EventMonitor
   * @param state The initial watch state for the monitor
   * @param delay Maximum duration that the monitor will poll the watch service for events
   * @param antiEntropy Minimum duration that must elapse before a build may by re-triggered by
   *                    the same file
   * @param terminationCondition Exits watch if evaluates to true
   * @param logger
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
    val eventThread = newEventsThread(delay, antiEntropy, state, events, logger)
    val userInputThread = newUserInputThread(terminationCondition, events, logger)
    new EventMonitorImpl(state.service, events, eventThread, userInputThread, logger, closeService)
  }

  private[io] def legacy(state: WatchState,
                         delay: FiniteDuration,
                         terminationCondition: => Boolean): EventMonitor =
    applyImpl(state, delay, 40.milliseconds, terminationCondition, NullLogger, closeService = false)

  private trait HasWatchState {
    def state(): WatchState
    def incrementCount(): Unit
  }
  private def newEventsThread(delay: FiniteDuration,
                              antiEntropy: FiniteDuration,
                              s: WatchState,
                              events: ArrayBlockingQueue[Event],
                              logger: Logger): Looper with HasWatchState = {
    var recentEvents = Map.empty[Path, Deadline]
    var registered = s.registered
    var count = s.count
    val lock = new Object
    new Looper(s"watch-state-event-thread-${eventThreadId.incrementAndGet()}") with HasWatchState {
      def incrementCount(): Unit = lock.synchronized { count += 1 }
      def state(): WatchState = lock.synchronized(s.withCount(count).withRegistered(registered))
      override def loop(): Unit = {
        recentEvents = recentEvents.filterNot(_._2.isOverdue)
        getFilesForKey(s.service.poll(delay)).foreach(maybeTrigger)
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
      /*
       * Triggers only if there is no pending Trigger and the file is not in an anti-entropy
       * quarantine.
       */
      private def maybeTrigger(path: Path): Unit =
        if (s.accept(path)) {
          if (recentEvents.get(path).fold(false)(!_.isOverdue))
            logger.debug(s"Ignoring watch event for $path due to anti-entropy constraint")
          else
            events.peek() match {
              case Cancelled =>
                logger.debug(s"Watch cancelled, not offering event for path $path")
              case _ =>
                recentEvents += path -> antiEntropy.fromNow
                if (!events.offer(Triggered(path))) {
                  logger.debug(s"Event already pending, dropping event for path: $path")
                }
            }
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

  private abstract class Looper(name: String) extends Thread(name) with AutoCloseable {
    private[this] var stopped = false
    private[this] var started = false
    private[this] val lock = new Object()
    def isStopped: Boolean = this.synchronized(stopped)
    def loop(): Unit
    @tailrec
    private final def runImpl(firstTime: Boolean): Unit = {
      if (firstTime) {
        started = true
        lock.synchronized(lock.notifyAll)
      }
      try {
        if (!isStopped) {
          loop()
        }
      } catch {
        case (_: ClosedWatchServiceException | _: InterruptedException) =>
          this.synchronized { stopped = true }
      }
      if (!isStopped) {
        runImpl(firstTime = false)
      }
    }
    override final def run(): Unit = runImpl(firstTime = true)
    def close(): Unit = this.synchronized {
      if (!stopped) {
        stopped = true
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
