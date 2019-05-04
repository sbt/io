package sbt.internal.io

import sbt.internal.nio.PollingWatchService

import scala.concurrent.duration._

class PollingWatchServiceSpec
    extends SourceModificationWatchSpec(
      (d: FiniteDuration) => new PollingWatchService(d),
      DefaultWatchServiceSpec.pollDelay
    )
