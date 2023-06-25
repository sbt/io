/*
 * sbt IO
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt.internal.io

import sbt.internal.nio.PollingWatchService

import scala.concurrent.duration._

class PollingWatchServiceSpec
    extends SourceModificationWatchSpec(
      (d: FiniteDuration) => new PollingWatchService(d),
      DefaultWatchServiceSpec.pollDelay
    )
