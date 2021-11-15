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

import java.nio.file._
import org.scalatest.flatspec.AnyFlatSpec
import sbt.io.syntax._

class CopyDirectorySpec extends AnyFlatSpec {
  it should "copy symlinks" in IO.withTemporaryDirectory { dir =>
    // Given:
    // src/
    //     actual/
    //         a.txt
    //     lib/
    //         a.txt -> ../actual/a.txt

    val srcFile1 = dir / "src" / "actual" / "a.txt"
    val srcFile2 = dir / "src" / "lib" / "a.txt"

    IO.write(srcFile1, "this is the file contents")

    IO.createDirectory(srcFile2.getParentFile)
    Files.createSymbolicLink(srcFile2.toPath, Paths.get("../actual/a.txt"))

    // When: the "src" directory is copied to "dst"
    IO.copyDirectory(dir / "src", dir / "dst")

    // Then: dst/lib/a.txt should have been created and have the correct contents
    assert(IO.read(dir / "dst" / "lib" / "a.txt") == "this is the file contents")
  }
}
