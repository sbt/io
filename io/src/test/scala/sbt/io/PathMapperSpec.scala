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

import java.nio.file.{ Files, Path => NioPath }

import org.scalatest.Outcome
import org.scalatest.flatspec
import org.scalatest.matchers.should.Matchers
import Path._
import sbt.io.syntax._

class PathMapperSpec extends flatspec.FixtureAnyFlatSpec with Matchers {

  type FixtureParam = NioPath

  "rebase | flat" should "copy resource mappings correctly" in { tempDirectory =>
    val base = tempDirectory.toFile

    val files = Seq(base / "src" / "main" / "resources" / "scalac-plugin.xml")
    val dirs = Seq(
      base / "src" / "main" / "resources",
      base / "target" / "scala-2.11" / "resource_managed" / "main"
    )
    val target = base / "target" / "scala-2.11" / "classes"

    val mappings = (files --- dirs) pair (file =>
      rebase(dirs, target)(file) orElse (flat(target): File => Option[File])(file)
    )

    mappings shouldBe Seq(
      base / "src" / "main" / "resources" / "scalac-plugin.xml" ->
        base / "target" / "scala-2.11" / "classes" / "scalac-plugin.xml"
    )
  }

  override protected def withFixture(test: OneArgTest): Outcome = {
    val tmpDir = Files.createTempDirectory("path-mappings")
    try {
      withFixture(test.toNoArgTest(tmpDir))
    } finally {
      // cleanup an delete the temp directory
      IO.delete(tmpDir.toFile)
    }
  }
}
