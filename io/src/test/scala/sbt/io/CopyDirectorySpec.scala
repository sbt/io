package sbt.io

import java.nio.file._
import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.syntax._

class CopyDirectorySpec extends FlatSpec with Matchers {
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
    IO.read(dir / "dst" / "lib" / "a.txt") shouldBe "this is the file contents"
  }
}
