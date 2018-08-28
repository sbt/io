package sbt.internal.io

import java.nio.file._

import sbt.io
import sbt.io.{ Path => _, _ }

import scala.annotation.tailrec
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
            logger: EventMonitor.Logger = NullLogger): EventMonitor =
    applyImpl(state, delay, antiEntropy, terminationCondition, new io.Logger {
      override def debug(msg: => Any): Unit = logger.debug(msg)
    }, closeService = true)

  private[EventMonitor] def applyImpl(watchState: WatchState,
                                      delay: FiniteDuration,
                                      antiEntropy: FiniteDuration,
                                      terminationCondition: => Boolean,
                                      logger: sbt.io.Logger,
                                      closeService: Boolean): EventMonitor = {
    val observable = new WatchServiceBackedObservable[Path](watchState,
                                                            delay,
                                                            (_: TypedPath).getPath,
                                                            closeService,
                                                            logger)
    val monitor = FileEventMonitor.antiEntropy(observable, antiEntropy, logger)
    new EventMonitor {
      private[this] var count = watchState.count

      /** Block indefinitely until the trigger receives a file event or the user stops the watch. */
      @tailrec
      override final def awaitEvent(): Boolean = {
        val triggeredPath = monitor
          .poll(10.millis)
          .find(p => watchState.sources.exists(s => s.accept(p.entry.getPath)))
        triggeredPath match {
          case Some(p) =>
            logger.debug(s"Triggered by ${p.entry.getPath}")
            count += 1
            true
          case _ if terminationCondition => false
          case _                         => awaitEvent()
        }
      }

      /** A snapshot of the WatchState that includes the number of build triggers and watch sources. */
      override def state(): WatchState = watchState.withCount(count)

      override def close(): Unit = monitor.close()
    }
  }

  private[io] def legacy(state: WatchState,
                         delay: FiniteDuration,
                         terminationCondition: => Boolean): EventMonitor =
    applyImpl(
      state,
      delay,
      200.milliseconds,
      terminationCondition,
      sbt.io.NullLogger,
      closeService = false
    )

  // Shutup the compiler about unused arguments
  @inline private[this] def ignoreArg(arg: => Any): Unit = if (true) () else { arg; () }
  trait Logger {
    def debug(msg: => Any): Unit
  }
  object NullLogger extends Logger {
    override def debug(msg: => Any): Unit = ignoreArg(msg)
  }
}
