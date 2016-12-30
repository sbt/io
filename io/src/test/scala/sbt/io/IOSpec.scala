package sbt.io

import java.io.File
import java.nio.file.Files
import org.scalatest.{ FlatSpec, Matchers }

class IOSpec extends FlatSpec with Matchers {
  it should "relativize" in {
    // Given:
    // io-relativize/
    //     meh.file
    //     inside-dir/
    //
    // and
    // relativeRootDir referring to io-relativize/inside-dir/../

    val rootDir = Files.createTempDirectory("io-relativize")
    val nestedFile = Files.createFile(rootDir resolve "meh.file").toFile
    val nestedDir = Files.createDirectory(rootDir resolve "inside-dir").toFile

    val relativeRootDir = new File(nestedDir, "..")

    IO.relativize(rootDir.toFile, nestedFile) shouldBe Some("meh.file")
    IO.relativize(relativeRootDir, nestedFile) shouldBe Some("../../meh.file")
  }
}
