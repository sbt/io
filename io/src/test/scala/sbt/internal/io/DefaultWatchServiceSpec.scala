package sbt.internal.io

import java.nio.file.FileSystems

import scala.concurrent.duration._

object DefaultWatchServiceSpec {
  // java.nio's default watch service is much slower on MacOS at the moment.
  // We give it more time to detect changes.
  val (pollDelay, maxWaitTime) =
    Option(sys.props("os.name")) match {
      case Some("Mac OS X") => (1.second, 15.seconds)
      case _                => (50.milliseconds, 3.seconds)
    }
}
class DefaultWatchServiceSpec extends SourceModificationWatchSpec(FileSystems.getDefault.newWatchService,
                                                                  DefaultWatchServiceSpec.pollDelay,
                                                                  DefaultWatchServiceSpec.maxWaitTime)

