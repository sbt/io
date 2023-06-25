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

import java.nio.file.FileSystems

import scala.concurrent.duration._
import scala.util.Properties

object DefaultWatchServiceSpec {
  val pollDelay = 100.milliseconds
}

class DefaultWatchServiceSpec
    extends SourceModificationWatchSpec(
      _ => if (Properties.isMac) new MacOSXWatchService else FileSystems.getDefault.newWatchService,
      DefaultWatchServiceSpec.pollDelay
    )
