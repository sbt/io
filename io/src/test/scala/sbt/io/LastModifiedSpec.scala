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

import java.nio.file.Files

import org.scalatest.FlatSpec

class LastModifiedSpec extends FlatSpec {
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
}
