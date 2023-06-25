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

package sbt.nio

import java.nio.file.FileSystems

import scala.collection.JavaConverters._

object TestHelpers {
  val root = FileSystems.getDefault.getRootDirectories.asScala.head
  val basePath = root.resolve("foo").resolve("bar")
}
