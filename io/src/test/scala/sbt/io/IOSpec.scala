package sbt.io

import java.io.File
import java.nio.file.Files
import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.syntax._

class IOSpec extends FlatSpec with Matchers {
  "IO" should "relativize" in {
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

  it should "relativize . dirs" in {
    val base = new File(".")
    val file1 = new File("./.git")
    val file2 = new File(".", ".git")
    val file3 = new File(base, ".git")

    IO.relativize(base, file1) shouldBe Some(".git")
    IO.relativize(base, file2) shouldBe Some(".git")
    IO.relativize(base, file3) shouldBe Some(".git")
  }

  "toURI" should "make URI" in {
    val u = IO.toURI(file("/etc/hosts"))
    assert(u.toString == "file:///etc/hosts")
  }

  it should "make u0 URI from a relative path" in {
    val u = IO.toURI(file("src/main/scala"))
    assert(u.toString == "file:src/main/scala")
  }

  it should "make URI that roundtrips" in {
    val u = IO.toURI(file("/etc/hosts"))
    assert(IO.toFile(u) == file("/etc/hosts"))
  }

  it should "make u0 URI that roundtrips" in {
    val u = IO.toURI(file("src/main/scala"))
    assert(IO.toFile(u) == file("src/main/scala"))
  }
}
