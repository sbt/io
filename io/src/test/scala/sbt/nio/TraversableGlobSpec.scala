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
import sbt.nio.file.{ FileTreeView, Glob, RecursiveGlob }

class TraversableGlobSpec extends AnyFlatSpec {
  "Traversable globs" should "collect multiple directories" in {
    IO.withTemporaryDirectory { dirFile =>
      val dir = dirFile.toPath
      val subdir = Files.createDirectories(dir.resolve("subdir"))
      val otherSubdir = Files.createDirectories(dir.resolve("other-subdir"))
      val nestedSubdir = Files.createDirectory(otherSubdir.resolve("nested"))
      val subdirs = Seq(subdir, otherSubdir, nestedSubdir)
      val files = subdirs.map(d => Files.createFile(d.resolve("file")))
      val globs = subdirs.dropRight(1).map(d => Glob(d, RecursiveGlob))

      val actual = FileTreeView.default.list(globs).map(_._1).toSet
      val expected = (files :+ nestedSubdir).toSet
      assert(actual == expected)
    }
  }
  it should "handle redundant globs" in {
    IO.withTemporaryDirectory { dirFile =>
      val dir = dirFile.toPath
      val subdir = Files.createDirectories(dir.resolve("subdir"))
      val file = Files.createFile(subdir.resolve("file.txt"))
      val globs = Seq[Glob](s"$dir/**", s"$dir/**/*.txt", s"$subdir/*/*.txt", Glob(file))
      val actual = FileTreeView.default.list(globs).map(_._1).sorted
      val expected = Seq(subdir, file)
      assert(actual == expected)
    }
  }
  it should "handle semi-overlapping globs" in {
    IO.withTemporaryDirectory { dirFile =>
      val dir = dirFile.toPath
      val subdir = Files.createDirectories(dir.resolve("subdir"))
      val nested = Files.createDirectories(subdir.resolve("nested"))
      val deeply = Files.createDirectories(nested.resolve("deeply"))
      val txtFile = Files.createFile(nested.resolve("file.txt"))
      val mdFile = Files.createFile(deeply.resolve("file.md"))
      val globs = Seq[Glob](s"$dir/**/*.md", s"$subdir/*.txt", s"$nested/*.md", s"$nested/*.txt")
      val actual = FileTreeView.default.list(globs).map(_._1).sorted
      val expected = Seq(mdFile, txtFile)
      assert(actual == expected)
    }
  }
}
