package sbt.internal.io

import java.nio.file.{ ClosedWatchServiceException, Files, Path, WatchKey }
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }

import sbt.io.WatchService

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration._

private[sbt] sealed trait EventMonitor extends AutoCloseable {

  /** A snapshot of the WatchState that includes the number of build triggers and watch sources. */
  def state(): WatchState

  /** Block indefinitely until the monitor receives a file event or the user stops the watch. */
  def watch(): Boolean

  /** Cleans up any service and/or threads started by the monitor */
  override def close(): Unit = close(closeService = true)
  /*
   * Workaround for the legacy implementation of SourceModificationWatch.watch
   */
  private[io] def close(closeService: Boolean): Unit
}

object EventMonitor {
  private sealed trait Event
  private case object Cancelled extends Event
  private case class Triggered(path: Path) extends Event

  private class EventMonitorImpl private[EventMonitor] (
      private[this] val service: WatchService,
      private[this] val events: ArrayBlockingQueue[Event],
      private[this] val eventThread: Looper with HasWatchState,
      private[this] val userInputThread: Looper,
      private[this] val logger: Logger)
      extends EventMonitor {

    override def state(): WatchState = eventThread.state()

    override def watch(): Boolean = events.take() match {
      case Cancelled => false
      case Triggered(path) =>
        logger.debug(s"Triggered watch event due to updated path: $path")
        eventThread.incrementCount()
        true
    }

    override def close(closeState: Boolean): Unit = {
      if (closed.compareAndSet(false, true)) {
        if (closeState) service.close()
        userInputThread.close()
        eventThread.close()
        logger.debug("Closed EventMonitor")
      }
    }

    private[this] val closed = new AtomicBoolean(false)
  }

  def apply(state: WatchState,
            delay: FiniteDuration,
            antiEntropy: FiniteDuration,
            terminationCondition: => Boolean,
            logger: Logger = NullLogger): EventMonitor = {
    val events = new ArrayBlockingQueue[Event](1)
    val eventThread = newEventsThread(delay, antiEntropy, state, events, logger)
    val userInputThread = newUserInputThread(terminationCondition, events, logger)
    new EventMonitorImpl(state.service, events, eventThread, userInputThread, logger)
  }

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
    new Looper(s"watch-state-event-thread-${eventThreadId.incrementAndGet()}") with HasWatchState {
      private[this] val lock = new Object
      private[this] var count = s.count
      private[this] var registered = s.registered
      def incrementCount(): Unit = lock.synchronized { count += 1 }
      def state(): WatchState = lock.synchronized(s.withCount(count).withRegistered(registered))
      override def loop(): Unit = {
        recentEvents = recentEvents.filterNot(_._2.isOverdue)
        getFilesForKey(s.service.poll(delay)).foreach(maybeTrigger)
      }
      def getFilesForKey(key: WatchKey): Seq[Path] = key match {
        case null => Nil
        case k =>
          val allEvents = k.pollEvents.asScala
            .map(e => k.watchable.asInstanceOf[Path].resolve(e.context.asInstanceOf[Path]))
          logger.debug(s"Received events:\n${allEvents.mkString("\n")}")
          val (exist, notExist) = allEvents.partition(Files.exists(_))
          val (updatedDirectories, updatedFiles) = exist.partition(Files.isDirectory(_))
          val newFiles = updatedDirectories.flatMap(filesForNewDirectory)
          lock.synchronized { registered --= notExist }
          notExist.foreach(s.unregister)
          updatedFiles ++ newFiles ++ notExist
      }
      /*
       * Returns new files found in new directory and any subdirectories, assuming that there is
       * a recursive source with a base that is parent to the directory.
       */
      def filesForNewDirectory(dir: Path): Seq[Path] = {
        lazy val recursive =
          s.sources.exists(src => dir.startsWith(src.base.toPath) && src.recursive)
        if (!registered.contains(dir) && recursive) {
          val dirs = Files.walk(dir).iterator.asScala.filter(Files.isDirectory(_))
          val newDirs = dirs.map(d => d -> s.register(d)).toIndexedSeq
          lock.synchronized { registered ++= newDirs }
          Files.walk(dir).iterator.asScala.toSeq
        } else Nil
      }
      /*
       * Triggers only if there is no pending Trigger and the file is not in an anti-entropy
       * quarantine.
       */
      def maybeTrigger(path: Path): Unit =
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
        if (terminationCondition) {
          logger.debug("Received termination condition. Stopping watch...")
          while (!events.offer(Cancelled)) {
            events.clear()
          }
        } else {}
      }
    }

  private abstract class Looper(name: String) extends Thread(name) with AutoCloseable {
    private[this] var stopped = false
    def isStopped: Boolean = this.synchronized(stopped)
    def loop(): Unit
    @tailrec
    override final def run(): Unit = {
      try {
        if (!isStopped) {
          loop()
        }
      } catch {
        case (_: ClosedWatchServiceException | _: InterruptedException) =>
          this.synchronized { stopped = true }
      }
      if (!isStopped) {
        run()
      }
    }
    def close(): Unit = this.synchronized {
      if (!stopped) {
        stopped = true
        this.interrupt()
        this.join(5000)
      }
    }
    setDaemon(true)
    start()
  }
  private val eventThreadId = new AtomicInteger(0)
  private val userInputId = new AtomicInteger(0)

}
