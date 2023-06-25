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

package object file {
  val * = sbt.nio.file.RelativeGlob.*
  val ** = sbt.nio.file.RelativeGlob.**
}
