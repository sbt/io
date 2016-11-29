package sbt.io

import java.io.File
import java.nio.file.Files
import org.scalatest.{ FlatSpec, Matchers }

class IOSpec extends FlatSpec with Matchers {
  it should "relativize" in {
    val rootDir = Files.createTempDirectory("io-relativize")
    val nestedFile = Files.createTempFile(rootDir, "meh", "file").toFile
    val nestedDir = Files.createTempDirectory(rootDir, "inside-dir").toFile

    val relativeRootDir = new File(nestedDir, "..")

    IO.relativize(rootDir.toFile, nestedFile).isDefined shouldBe true
    IO.relativize(relativeRootDir, nestedFile).isDefined shouldBe true
  }
}
