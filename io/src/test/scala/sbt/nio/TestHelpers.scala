/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio

import java.nio.file.FileSystems

import scala.collection.JavaConverters._

object TestHelpers {
  val root = FileSystems.getDefault.getRootDirectories.asScala.head
  val basePath = root.resolve("foo").resolve("bar")
}
