package sbt.internal.io

import sbt.io.PollingWatchService

class PollingWatchServiceSpec extends SourceModificationWatchSpec(new PollingWatchService(500L), 500L, 3000L)

