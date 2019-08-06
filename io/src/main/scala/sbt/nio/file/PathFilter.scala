/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio.file

import java.io.FileFilter
import java.nio.file.{ Files, Path }

/**
 * A filter for a path and its attributes.
 */
trait PathFilter {

  /**
   *
   * @param path the path name
   * @param attributes the file attributes corresponding to `path`
   * @return true if the filter accepts the path
   */
  def accept(path: Path, attributes: FileAttributes): Boolean
}

object PathFilter {

  /**
   * Returns a PathFilter that accepts any path for which there exists a [[Glob]] that matches
   * the path
   * @param globs the glob patterns
   * @return a PathFilter that accepts any path matching one or more input [[Glob]]s.
   */
  def apply(globs: Glob*): PathFilter = if (globs.isEmpty) NoPass else new GlobPathFilter(globs)

  /**
   * Provides extension methods for [[PathFilter]]
   * @param pathFilter the [[PathFilter]] to extend
   */
  implicit class Ops(val pathFilter: PathFilter) extends AnyVal {

    /**
     * Returns a combined [[PathFilter]] that returns true only if both [[PathFilter]] instances
     * accept the path.
     * @param other the other [[PathFilter]]
     */
    def &&(other: PathFilter): PathFilter = pathFilter match {
      case NoPass  => NoPass
      case AllPass => other
      case f =>
        other match {
          case NoPass  => NoPass
          case AllPass => f
          case g       => new AndPathFilter(f, g)
        }
    }

    /**
     * Returns a combined [[PathFilter]] that returns true only if both [[PathFilter]] instances
     * accept the path.
     * @param other the other [[PathFilter]]
     */
    def ||(other: PathFilter): PathFilter = pathFilter match {
      case NoPass  => other
      case AllPass => AllPass
      case f =>
        other match {
          case NoPass  => f
          case AllPass => AllPass
          case g       => new OrPathFilter(f, g)
        }
    }

    /**
     * Inverts the [[PathFilter.accept]] method.
     * @return
     */
    def unary_! : PathFilter = pathFilter match {
      case AllPass         => NoPass
      case NoPass          => AllPass
      case HiddenFilter    => NotHiddenFilter
      case NotHiddenFilter => HiddenFilter
      case pf              => new NotPathFilter(pf)
    }
  }

  /**
   * Converts an instance of [[sbt.io.FileFilter]] to an [[sbt.nio.file.PathFilter]]. It will
   * de-structure the [[sbt.io.FileFilter]] if possible to convert it to an equivalent, and
   * possibly more efficient, [[PathFilter]].
   *
   * @param fileFilter the filter to convert
   * @return the converted.
   */
  implicit def fromFileFilter(fileFilter: FileFilter): PathFilter = fileFilter match {
    case sbt.io.HiddenFileFilter    => HiddenFilter
    case sbt.io.NotHiddenFileFilter => NotHiddenFilter
    case sbt.io.AllPassFilter       => AllPass
    case sbt.io.NothingFilter       => NoPass
    case sbt.io.DirectoryFilter     => DirectoryFilter
    case nf: sbt.io.NotFilter       => !fromFileFilter(nf.fileFilter)
    case af: sbt.io.AndFilter       => fromFileFilter(af.left) && fromFileFilter(af.right)
    case of: sbt.io.OrFilter        => fromFileFilter(of.left) || fromFileFilter(of.right)
    case filter =>
      new PathFilter {
        override def accept(path: Path, attributes: FileAttributes): Boolean =
          filter.accept(path.toFile)
        override def toString: String = s"WrappedFileFilter($filter)"
      }
  }

  /**
   * Converts a function from `Path` to `Boolean` to a [[PathFilter]] by ignoring the
   * [[FileAttributes]] parameter to [[PathFilter.accept]].
   * @param filter the function to wrap
   * @return the [[PathFilter]].
   */
  def fromPathPredicate(filter: Path => Boolean): PathFilter =
    new PathFilter {
      override def accept(path: Path, attributes: FileAttributes): Boolean = filter(path)
      override def toString: String = s"WrappedPathPredicate($filter)"
    }

  /**
   * Converts a function from `FileAttributes` to `Boolean` to a [[PathFilter]] by ignoring the
   * `Path` parameter to [[PathFilter.accept]].
   * @param filter the function to wrap
   * @return the [[PathFilter]].
   */
  def fromAttributePredicate(filter: FileAttributes => Boolean): PathFilter =
    new PathFilter {
      override def accept(path: Path, attributes: FileAttributes): Boolean = filter(attributes)
      override def toString: String = s"WrappedPathPredicate($filter)"
    }
}

/**
 * A [[PathFilter]] that includes only directories.
 */
case object DirectoryFilter extends PathFilter {

  /**
   * Returns true if the `path` is a directory
   * @param path the path name
   * @param attributes the file attributes corresponding to `path`
   * @return true if the path is a directory.
   */
  override def accept(path: Path, attributes: FileAttributes): Boolean = attributes.isDirectory
}

/**
 * A [[PathFilter]] that includes only regular files.
 */
case object RegularFileFilter extends PathFilter {

  /**
   * Returns true if the `path` is a regular file
   * @param path the path name
   * @param attributes the file attributes corresponding to `path`
   * @return true if the path is a regular file.
   */
  override def accept(path: Path, attributes: FileAttributes): Boolean = attributes.isRegularFile
}

/**
 * A [[PathFilter]] that includes only hidden files according to
 * [[https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#isHidden-java.nio.file.Path- Files.isHidden]].
 */
case object HiddenFilter extends PathFilter {

  /**
   * Returns true if the `path` is hidden according to
   * [[https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#isHidden-java.nio.file.Path- Files.isHidden]].
   * @param path the path name
   * @param attributes the file attributes corresponding to `path`
   * @return true if the file is hidden.
   */
  override def accept(path: Path, attributes: FileAttributes): Boolean = Files.isHidden(path)
}

/**
 * A [[PathFilter]] that includes only files that are not hidden according to
 * [[https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#isHidden-java.nio.file.Path- Files.isHidden]].
 */
case object NotHiddenFilter extends PathFilter {
  override def accept(path: Path, attributes: FileAttributes): Boolean = !Files.isHidden(path)
}

case object AllPass extends PathFilter {

  /**
   * Always returns true.
   * @param path the path name
   * @param attributes the file attributes corresponding to `path`
   * @return true.
   */
  override def accept(path: Path, attributes: FileAttributes): Boolean = true
}

case object NoPass extends PathFilter {

  /**
   * Always returns false.
   * @param path the path name
   * @param attributes the file attributes corresponding to `path`
   * @return false.
   */
  override def accept(path: Path, attributes: FileAttributes): Boolean = false
}

private[sbt] class AndPathFilter(private val left: PathFilter, private val right: PathFilter)
    extends PathFilter {
  override def accept(path: Path, attributes: FileAttributes): Boolean =
    left.accept(path, attributes) && right.accept(path, attributes)
  override def equals(obj: Any): Boolean = obj match {
    case that: AndPathFilter => this.left == that.left && this.right == that.right
    case _                   => false
  }
  override def hashCode(): Int = (left.## * 31) ^ right.##
  override def toString: String = s"$left && $right"
}

private[sbt] class OrPathFilter(private val left: PathFilter, private val right: PathFilter)
    extends PathFilter {
  override def accept(path: Path, attributes: FileAttributes): Boolean =
    left.accept(path, attributes) || right.accept(path, attributes)
  override def hashCode: Int = (left.## * 31) ^ right.##
  override def equals(obj: Any): Boolean = obj match {
    case that: OrPathFilter => this.left == that.left && this.right == that.right
    case _                  => false
  }
  override def toString: String = s"$left || $right"
}

private[sbt] class NotPathFilter(private val filter: PathFilter) extends PathFilter {
  override def accept(path: Path, attributes: FileAttributes): Boolean =
    !filter.accept(path, attributes)
  override def equals(obj: Any): Boolean = obj match {
    case that: NotPathFilter => this.filter == that.filter
    case _                   => false
  }
  override def hashCode: Int = filter.##
  override def toString: String = s"!$filter"
}

private[sbt] class GlobPathFilter(private val globs: Seq[Glob]) extends PathFilter {
  override def accept(path: Path, attributes: FileAttributes): Boolean =
    globs.exists(_.matches(path))
  override def equals(obj: Any): Boolean = obj match {
    case that: GlobPathFilter => this.globs == that.globs
    case _                    => false
  }
  override def hashCode(): Int = globs.##
  override def toString: String = s"GlobPathFilter($globs)"
}
