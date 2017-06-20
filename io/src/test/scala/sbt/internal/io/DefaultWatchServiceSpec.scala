package sbt.internal.io

import java.nio.file.FileSystems

object DefaultWatchServiceSpec {
  val (pollDelayMs, maxWaitTimeMs) =
    Option(sys.props("os.name")) match {
      case Some("Mac OS X") => (200L, 15000L)
      case _                => (50L, 3000L)
    }
}
class DefaultWatchServiceSpec extends SourceModificationWatchSpec(FileSystems.getDefault.newWatchService, DefaultWatchServiceSpec.pollDelayMs, DefaultWatchServiceSpec.maxWaitTimeMs)

