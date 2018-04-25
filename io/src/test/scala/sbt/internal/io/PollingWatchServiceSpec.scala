package sbt.internal.io

import sbt.io.PollingWatchService

import scala.concurrent.duration._

class PollingWatchServiceSpec
    extends SourceModificationWatchSpec(new PollingWatchService(5.milliseconds),
                                        DefaultWatchServiceSpec.pollDelay)
