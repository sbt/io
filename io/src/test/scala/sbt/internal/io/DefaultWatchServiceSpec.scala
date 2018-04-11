package sbt.internal.io

import java.nio.file.FileSystems

import sbt.io.MacOSXWatchService

import scala.concurrent.duration._
import scala.util.Properties

object DefaultWatchServiceSpec {
  val pollDelay = 50.milliseconds
}

class DefaultWatchServiceSpec
    extends SourceModificationWatchSpec(
      if (Properties.isMac) new MacOSXWatchService else FileSystems.getDefault.newWatchService,
      DefaultWatchServiceSpec.pollDelay
    )
