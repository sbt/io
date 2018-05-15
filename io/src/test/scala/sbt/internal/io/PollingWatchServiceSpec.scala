package sbt.internal.io

import sbt.io.PollingWatchService

import scala.concurrent.duration._

class PollingWatchServiceSpec
    extends SourceModificationWatchSpec((d: FiniteDuration) => new PollingWatchService(d),
                                        DefaultWatchServiceSpec.pollDelay)
