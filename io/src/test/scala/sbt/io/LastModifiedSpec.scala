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

import java.nio.file.{ Files, Paths => JPaths }

import org.scalatest.flatspec.AnyFlatSpec
import sbt.nio.file.syntax._

class LastModifiedSpec extends AnyFlatSpec {
  "IO.getModifiedTimeOrZero" should "work with long path names" in IO.withTemporaryDirectory {
    dir =>
      val fileName = "a" * 32
      val nested =
        (1 to 8).foldLeft(dir.toPath) {
          case (d, _) => Files.createDirectories(d.resolve(fileName))
        }
      val file = Files.createFile(nested.resolve(fileName)).toFile
      // in case target platform only has second precision round to nearest second
      val lm = (System.currentTimeMillis / 1000) * 1000
      IO.setModifiedTimeOrFalse(file, lm)
      assert(IO.getModifiedTimeOrZero(file) == lm)
  }
  it should "handle empty paths" in {
    assert(IO.getModifiedTimeOrZero(JPaths.get("").toFile) > 0)
    val newLM = ((System.currentTimeMillis + 10000) / 1000) * 1000
    IO.setModifiedTimeOrFalse(JPaths.get("").toFile, newLM)
    assert(IO.getModifiedTimeOrZero(JPaths.get("").toFile) == newLM)
  }
  it should "handle relative paths" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val subDir1 = Files.createDirectories(dirPath / "subdir-1")
    val subDir2 = Files.createDirectories(dirPath / "subdir-2")
    val subFile = Files.createFile(subDir2 / "file")
    val lm = IO.getModifiedTimeOrZero(subFile.toFile)
    val relative = subDir1 / ".." / subDir2.getFileName.toString / subFile.getFileName.toString
    assert(IO.getModifiedTimeOrZero(relative.toFile) == lm)
    val newLM = ((System.currentTimeMillis + 10000) / 1000) * 1000
    IO.setModifiedTimeOrFalse(relative.toFile, newLM)
    assert(IO.getModifiedTimeOrZero(relative.toFile) == newLM)
  }
}
