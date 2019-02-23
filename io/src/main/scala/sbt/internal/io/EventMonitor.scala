/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.io

import java.nio.file._

import sbt.internal.io.FileEvent.{ Deletion, Update }
import sbt.io.{ Path => _ }

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Success

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
@deprecated("This has been removed in favor of FileEventMonitor", "1.3.0")
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

  /**
   * Create a new EventMonitor
   *
   * @param watchState           The initial watch state for the monitor
   * @param delay                Maximum duration that the monitor will poll the watch service for events
   * @param antiEntropy          Minimum duration that must elapse before a build may by re-triggered by
   *                             the same file
   * @param terminationCondition Exits watch if evaluates to true
   * @param logger               Logs output
   * @return The new EventMonitor
   */
  @deprecated("This method is no longer used in sbt. It exists for legacy binary compatibility " +
                "within the 1.x series.",
              "1.3.0")
  def apply(watchState: WatchState,
            delay: FiniteDuration,
            antiEntropy: FiniteDuration,
            terminationCondition: => Boolean,
            logger: Logger = NullLogger): EventMonitor = {
    val eventLogger = new WatchLogger {
      override def debug(msg: => Any): Unit = logger.debug(msg)
    }
    val converter: (Path, SimpleFileAttributes) => FileEvent[SimpleFileAttributes] = (p, a) =>
      if (a.exists) Update(p, a, a) else Deletion(p, a)
    val underlying: Observable[(Path, SimpleFileAttributes)] =
      new WatchServiceBackedObservable[SimpleFileAttributes](watchState.toNewWatchState,
                                                             delay,
                                                             (p, a) => Success(a),
                                                             closeService = true,
                                                             eventLogger)
    val observable: Observable[FileEvent[SimpleFileAttributes]] =
      Observable.map(underlying, converter.tupled)
    val monitor =
      FileEventMonitor.antiEntropy(observable, antiEntropy, eventLogger, 50.millis, 10.minutes)
    new EventMonitor {
      private[this] var count = watchState.count

      /** Block indefinitely until the monitor receives a file event or the user stops the watch. */
      @tailrec
      override final def awaitEvent(): Boolean = {
        val triggeredEvent = monitor
          .poll(10.millis)
          .find(e => watchState.sources.exists(s => s.accept(e.path)))
        triggeredEvent match {
          case Some(e) =>
            logger.debug(s"Triggered by ${e.path}")
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

  // Shutup the compiler about unused arguments
  @inline private[this] def ignoreArg(arg: => Any): Unit = if (true) () else { arg; () }
  trait Logger {
    def debug(msg: => Any): Unit = ignoreArg(msg)
  }
  object NullLogger extends Logger {
    override def debug(msg: => Any): Unit = ignoreArg(msg)
  }
}
