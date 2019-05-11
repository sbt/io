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
