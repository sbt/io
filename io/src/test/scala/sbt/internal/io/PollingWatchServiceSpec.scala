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

import sbt.internal.nio.PollingWatchService

import scala.concurrent.duration._

class PollingWatchServiceSpec
    extends SourceModificationWatchSpec(
      (d: FiniteDuration) => new PollingWatchService(d),
      DefaultWatchServiceSpec.pollDelay
    )
