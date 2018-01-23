package sbt.io

import java.nio.file._
import java.nio.file.attribute.PosixFilePermissions

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.syntax._
import scala.collection.JavaConverters._

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
    srcFile1.setPermissions(PosixFilePermissions.fromString("rwxrwxr-x").asScala.toSet) // 775

    IO.createDirectory(srcFile2.getParentFile)
    Files.createSymbolicLink(srcFile2.toPath, Paths.get("../actual/a.txt"))

    // When: the "src" directory is copied to "dst"
    IO.copyDirectory(dir / "src", dir / "dst")

    // Then: dst/lib/a.txt should have been created and have the correct contents
    IO.read(dir / "dst" / "lib" / "a.txt") shouldBe "this is the file contents"

    // Preserve the file permissions
    val dstFile1 = dir / "dst" / "lib" / "a.txt"
    dstFile1.isOwnerReadable shouldBe srcFile1.isOwnerReadable
    dstFile1.isOwnerWritable shouldBe srcFile1.isOwnerWritable
    dstFile1.isOwnerExecutable shouldBe srcFile1.isOwnerExecutable
    dstFile1.isGroupReadable shouldBe srcFile1.isGroupReadable
    dstFile1.isGroupWritable shouldBe srcFile1.isGroupWritable
    dstFile1.isGroupExecutable shouldBe srcFile1.isGroupExecutable
    dstFile1.isOthersReadable shouldBe srcFile1.isOthersReadable
    dstFile1.isOthersWritable shouldBe srcFile1.isOthersWritable
    dstFile1.isOthersExecutable shouldBe srcFile1.isOthersExecutable
  }
}
