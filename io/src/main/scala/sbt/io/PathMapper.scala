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

import java.io.File

abstract class Mapper {
  type PathMap = File => Option[String]
  type FileMap = File => Option[File]

  /** A path mapper that pairs a File with the path returned by calling `getPath` on it. */
  val basic: PathMap = f => Some(f.getPath)

  /**
   * A path mapper that pairs a File with its path relative to `base`.
   * If the File is not a descendant of `base`, it is not handled (None is returned by the mapper).
   */
  def relativeTo(base: File): PathMap = IO.relativize(base, _)

  def relativeTo(bases: Iterable[File], zero: PathMap = transparent): PathMap =
    fold(zero, bases)(relativeTo)

  /**
   * A path mapper that pairs a descendent of `oldBase` with `newBase` prepended to the path relative to `oldBase`.
   * For example, if `oldBase = /old/x/` and `newBase = new/a/`, then `/old/x/y/z.txt` gets paired with `new/a/y/z.txt`.
   */
  def rebase(oldBase: File, newBase: String): PathMap = {
    val normNewBase = normalizeBase(newBase)
    (file: File) =>
      if (file == oldBase)
        Some(if (normNewBase.isEmpty) "." else normNewBase)
      else
        IO.relativize(oldBase, file).map(normNewBase + _)
  }

  /**
   * A mapper that throws an exception for any input.
   * This is useful as the last mapper in a pipeline to ensure every input gets mapped.
   */
  def fail: Any => Nothing = f => sys.error("No mapping for " + f)

  /** A path mapper that pairs a File with its name.  For example, `/x/y/z.txt` gets paired with `z.txt`. */
  val flat: PathMap = f => Some(f.getName)

  /**
   * A path mapper that pairs a File with a path constructed from `newBase` and the file's name.
   * For example, if `newBase = /new/a/`, then `/old/x/z.txt` gets paired with `/new/a/z.txt`.
   */
  def flatRebase(newBase: String): PathMap = {
    val newBase0 = normalizeBase(newBase)
    (f => Some(newBase0 + f.getName))
  }

  /** A mapper that is defined on all inputs by the function `f`. */
  def total[A, B](f: A => B): A => Some[B] = x => Some(f(x))

  /** A mapper that ignores all inputs. */
  def transparent: Any => Option[Nothing] = _ => None

  def normalizeBase(base: String) = if (!base.isEmpty && !base.endsWith("/")) base + "/" else base

  /**
   * Pairs a File with the absolute File obtained by calling `getAbsoluteFile`.
   * Note that this usually means that relative files are resolved against the current working directory.
   */
  def abs: FileMap = f => Some(f.getAbsoluteFile)

  /**
   * Returns a FileMap that resolves a relative File against `newDirectory`
   * and pairs the original File with the resolved File.
   * The mapper ignores absolute files.
   */
  def resolve(newDirectory: File): FileMap =
    file => if (file.isAbsolute) None else Some(new File(newDirectory, file.getPath))

  def rebase(oldBases: Iterable[File], newBase: File, zero: FileMap = transparent): FileMap =
    fold(zero, oldBases)(old => rebase(old, newBase))

  /**
   * Produces a File mapper that pairs a descendant of `oldBase` with a file in `newBase` that preserving the relative path of the original file against `oldBase`.
   * For example, if `oldBase` is `/old/x/` and `newBase` is `/new/a/`, `/old/x/y/z.txt` gets paired with `/new/a/y/z.txt`.
   */
  def rebase(oldBase: File, newBase: File): FileMap =
    file =>
      if (file == oldBase)
        Some(newBase)
      else
        IO.relativize(oldBase, file) map (r => new File(newBase, r))

  /**
   * Constructs a FileMap that pairs a file with a file with the same name in `newDirectory`.
   * For example, if `newDirectory` is `/a/b`, then `/r/s/t/d.txt` will be paired with `/a/b/d.txt`
   */
  def flat(newDirectory: File): FileMap = file => Some(new File(newDirectory, file.getName))

  /**
   * Selects all descendants of `base` directory and maps them to a path relative to `base`.
   * `base` itself is not included.
   */
  def allSubpaths(base: File): Traversable[(File, String)] =
    selectSubpaths(base, AllPassFilter)

  /**
   * Selects descendants of `base` directory matching `filter` and maps them to a path relative to `base`.
   * `base` itself is not included.
   */
  def selectSubpaths(base: File, filter: FileFilter): Traversable[(File, String)] =
    PathFinder(base).globRecursive(filter).get().collect {
      case f if f != base => f -> base.toPath.relativize(f.toPath).toString
    }

  /**
   * return a Seq of mappings which effect is to add a whole directory in the generated package
   *
   * @example In order to create mappings for a static directory "extra" add
   * {{{
   * mappings ++= directory(baseDirectory.value / "extra")
   * }}}
   *
   * The resulting mappings sequence will look something like this
   *
   * {{{
   * File(baseDirectory/extras) -> "extras"
   * File(baseDirectory/extras/file1) -> "extras/file1"
   * File(baseDirectory/extras/file2) -> "extras/file2"
   * ...
   * }}}
   *
   * @param baseDirectory The directory that should be turned into a mappings sequence.
   * @return mappings The `baseDirectory` and all of its contents
   */
  def directory(baseDirectory: File): Seq[(File, String)] =
    Option(baseDirectory.getParentFile)
      .map(parent => PathFinder(baseDirectory).allPaths pair relativeTo(parent))
      .getOrElse(PathFinder(baseDirectory).allPaths pair basic)

  /**
   * return a Seq of mappings  excluding the directory itself.
   *
   * @example In order to create mappings for a static directory "extra" add
   * {{{
   * mappings ++= contentOf(baseDirectory.value / "extra")
   * }}}
   *
   * The resulting mappings sequence will look something like this
   *
   * {{{
   * File(baseDirectory/extras/file1) -> "file1"
   * File(baseDirectory/extras/file2) -> "file2"
   * ...
   * }}}
   *
   * @example Add a static directory "extra" and re-map the destination to a different path
   * {{{
   * mappings ++= contentOf(baseDirectory.value / "extra").map {
   *   case (src, destination) => src -> s"new/path/destination"
   * }
   * }}}
   *
   * @param baseDirectory The directory that should be turned into a mappings sequence.
   * @return mappings - The `basicDirectory`'s contents exlcuding `basicDirectory` itself
   */
  def contentOf(baseDirectory: File): Seq[(File, String)] = (
    (PathFinder(baseDirectory).allPaths --- PathFinder(baseDirectory))
      pair relativeTo(baseDirectory)
  )

  private[this] def fold[A, B, T](zero: A => Option[B], in: Iterable[T])(
      f: T => A => Option[B]
  ): A => Option[B] =
    in.foldLeft(zero)((mapper, base) => a => f(base)(a) orElse mapper(a))
}
