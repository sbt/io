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

import scala.util.Try
import org.scalacheck._, Prop._
import java.io.IOException
import java.nio.file.Files

object IOSpecification extends Properties("IO") {
  property("IO.classLocationPath able to determine containing directories") = forAll(classes) {
    (c: Class[?]) =>
      Try(IO.classLocationPath(c)).toOption.exists {
        case jar if jar.getFileName.toString.endsWith(".jar") =>
          Files.isRegularFile(jar)
        case jrt if jrt.getFileSystem.provider.getScheme == "jrt" =>
          jrt.toString.contains("/java.base")
        case dir =>
          Files.isDirectory(dir)
      }
  }

  property("IO.transfer to File writes completed content") = secure {
    val dir = Files.createTempDirectory("sbt-io-transfer-success").toFile
    val target = new java.io.File(dir, "target.txt")
    try {
      IO.transfer(new java.io.ByteArrayInputStream("complete".getBytes(IO.utf8)), target)
      val content = IO.read(target)
      content ?= "complete"
    } finally IO.delete(dir)
  }

  property("IO.transfer to File preserves existing destination on failure") = secure {
    val dir = Files.createTempDirectory("sbt-io-transfer-failure").toFile
    val target = new java.io.File(dir, "target.txt")
    try {
      IO.write(target, "previous")
      val failed = Try {
        IO.transfer(new FailingInputStream("partial".getBytes(IO.utf8), 3), target)
      }.isFailure
      val content = IO.read(target)
      (failed ?= true) && (content ?= "previous")
    } finally IO.delete(dir)
  }

  property("IO.copyFile writes completed content") = secure {
    val dir = Files.createTempDirectory("sbt-io-copy-file-success").toFile
    val source = new java.io.File(dir, "source.txt")
    val target = new java.io.File(dir, "target.txt")
    try {
      IO.write(source, "complete")
      val p1 =
        if (IO.isWindows) None
        else Some(Files.getPosixFilePermissions(source.toPath()))
      IO.copyFile(source, target)
      val p2 =
        if (IO.isWindows) None
        else Some(Files.getPosixFilePermissions(target.toPath()))
      val content = IO.read(target)
      (content ?= "complete") && (p1 == p2)
    } finally IO.delete(dir)
  }

  property("IO.copyFile preserves existing destination on failure") = secure {
    val dir = Files.createTempDirectory("sbt-io-copy-file-failure").toFile
    val source = new java.io.File(dir, "missing-source.txt")
    val target = new java.io.File(dir, "target.txt")
    try {
      IO.write(target, "previous")
      val failed = Try(IO.copyFile(source, target)).isFailure
      val content = IO.read(target)
      (failed ?= true) && (content ?= "previous")
    } finally IO.delete(dir)
  }

  property("IO.jar preserves existing destination on failure") = secure {
    val dir = Files.createTempDirectory("sbt-io-jar-failure").toFile
    val target = new java.io.File(dir, "out.jar")
    val phantom = new java.io.File(dir, "phantom.txt") {
      override def isFile: Boolean = true
    }
    try {
      IO.write(target, "previous")
      val failed = Try(
        IO.jar(Seq(phantom -> "phantom.txt"), target, new java.util.jar.Manifest, None)
      ).isFailure
      val content = IO.read(target)
      IO.isWindows || ((failed ?= true) && (content ?= "previous"))
    } finally IO.delete(dir)
  }

  implicit def classes: Gen[Class[?]] =
    Gen.oneOf(
      this.getClass,
      classOf[java.lang.Integer],
      classOf[java.util.AbstractMap.SimpleEntry[String, String]],
      classOf[String],
      classOf[Thread],
      classOf[Properties]
    )

  final class FailingInputStream(bytes: Array[Byte], failAt: Int) extends java.io.InputStream {
    private var index = 0
    override def read(): Int =
      if (index == failAt) throw new IOException("simulated failure")
      else if (index >= bytes.length) -1
      else {
        val b = bytes(index) & 0xff
        index += 1
        b
      }
  }
}
