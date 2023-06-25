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
import org.scalatest.matchers.should.Matchers
import sbt.io.syntax._
import java.nio.file.attribute.PosixFilePermission

class FileSpec extends AnyFlatSpec with Matchers {
  "files" should "set/unset permissions" in {
    IO.withTemporaryDirectory { dir =>
      val t1 = dir / "foo.txt"
      IO.write(t1, "foo")
      if (IO.isPosix) {
        t1.permissions(PosixFilePermission.OWNER_EXECUTE) shouldBe false

        t1.addPermission(PosixFilePermission.OWNER_EXECUTE)
        t1.addPermission(PosixFilePermission.GROUP_WRITE)
        t1.testPermission(PosixFilePermission.OWNER_EXECUTE) shouldBe true
        t1.permissionsAsString should fullyMatch regex "..x.w...."

        t1.removePermission(PosixFilePermission.OWNER_EXECUTE)
        t1.isOwnerExecutable shouldBe false
        t1.permissionsAsString should fullyMatch regex "..-.w...."
      } else ()
    }
  }
}
