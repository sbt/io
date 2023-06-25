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

package sbt.io

import org.scalatest.flatspec.AnyFlatSpec

final class JavaMilliSpec extends AnyFlatSpec {
  "JavaMilli" should "be exposed to the sbt.io package" in
    assertCompiles("""sbt.io.JavaMilli.getModifiedTime("/tmp")""")
}
