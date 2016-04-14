package sbt.io

import java.io.File
import java.nio.file.Files

import org.scalatest.{ FlatSpec, Matchers }

/**
 * Created by Lloyd on 4/14/16.
 *
 * Copyright 2016
 */
class IOSpec extends FlatSpec with Matchers {

  it should "relativize" in {
    val tempDir = Files.createTempDirectory("io-relativize")
    val tempFile = Files.createTempFile(tempDir, "meh", "file")
    val tempDirInDir = Files.createTempDirectory(tempDir, "inside-dir")

    val dirFromDirInDir = new File(tempDirInDir.toFile, "..")

    IO.relativize(tempDir.toFile, tempFile.toFile).isDefined shouldBe true
    IO.relativize(dirFromDirInDir, tempFile.toFile).isDefined shouldBe true
  }

}
