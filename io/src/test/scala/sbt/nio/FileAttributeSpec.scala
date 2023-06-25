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

package sbt.nio

import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec
import sbt.io.IO
import sbt.nio.file.FileAttributes

class FileAttributeSpec extends AnyFlatSpec {
  "symlinks" should "be resolved" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val file = Files.createFile(dirPath.resolve("file"))
    val link = Files.createSymbolicLink(dirPath.resolve("link"), file)
    FileAttributes(link).toOption match {
      case None => assert(false)
      case Some(a) =>
        assert(a.isRegularFile)
        assert(a.isSymbolicLink)
        assert(!a.isDirectory)
        assert(!a.isOther)
    }
  }
  "broken symlinks" should "have isSymbolicLink return true" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val file = dirPath.resolve("file")
    val link = Files.createSymbolicLink(dirPath.resolve("link"), file)
    FileAttributes(link).toOption match {
      case None => assert(false)
      case Some(a) =>
        assert(a.isSymbolicLink)
        assert(!a.isRegularFile)
        assert(!a.isDirectory)
        assert(!a.isOther)
    }
  }
  "symlinks" should "not be resolved with nofollow links" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val subdir = Files.createFile(dirPath.resolve("file"))
    val link = Files.createSymbolicLink(dirPath.resolve("link"), subdir)
    FileAttributes(link, followLinks = false).toOption match {
      case None => assert(false)
      case Some(a) =>
        assert(a.isSymbolicLink)
        assert(!a.isRegularFile)
        assert(!a.isDirectory)
        assert(!a.isOther)
    }
  }
  "broken symlinks" should "have isSymbolicLink true with no follow" in IO.withTemporaryDirectory {
    dir =>
      val dirPath = dir.toPath
      val file = dirPath.resolve("file")
      val link = Files.createSymbolicLink(dirPath.resolve("link"), file)
      FileAttributes(link, followLinks = false).toOption match {
        case None => assert(false)
        case Some(a) =>
          assert(a.isSymbolicLink)
          assert(!a.isRegularFile)
          assert(!a.isDirectory)
          assert(!a.isOther)
      }
  }
}
