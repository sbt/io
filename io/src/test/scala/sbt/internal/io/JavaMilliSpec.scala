/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.io

import org.scalatest.flatspec.AnyFlatSpec

final class JavaMilliSpec extends AnyFlatSpec {
  "JavaMilli" should "be exposed to the sbt.io package" in
    assertCompiles("""sbt.io.JavaMilli.getModifiedTime("/tmp")""")
}
