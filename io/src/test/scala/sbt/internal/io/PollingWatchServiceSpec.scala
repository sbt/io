package sbt.internal.io

import sbt.io.PollingWatchService

import scala.concurrent.duration._

class PollingWatchServiceSpec extends SourceModificationWatchSpec(new PollingWatchService(500.milliseconds),
                                                                  500.milliseconds,
                                                                  3.seconds)

