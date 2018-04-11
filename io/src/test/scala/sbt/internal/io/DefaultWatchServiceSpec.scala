package sbt.internal.io

import java.nio.file.FileSystems

import sbt.io.MacOSXWatchService

import scala.concurrent.duration._
import scala.util.Properties

object DefaultWatchServiceSpec {
  // java.nio's default watch service is much slower on MacOS at the moment.
  // We give it more time to detect changes.
  val (pollDelay, maxWaitTime) = (50.milliseconds, 3.seconds)
}

class DefaultWatchServiceSpec
    extends SourceModificationWatchSpec(
      if (Properties.isMac) new MacOSXWatchService else FileSystems.getDefault.newWatchService,
      DefaultWatchServiceSpec.pollDelay,
      DefaultWatchServiceSpec.maxWaitTime
    )
