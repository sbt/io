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

private[sbt] trait LowPriorityPathFilter {

  /**
   * Converts a glob string to a [[sbt.nio.file.PathFilter]].
   * @param glob the glob string to convert to a filter, e.g. "**<code>/</code>*.scala"
   * @return the [[PathFilter]] corresponding to the parsed [[Glob]]. May throw an exception if the
   *         glob string can not be parsed into a [[Glob]].
   */
  implicit def stringToPathFilter(glob: String): PathFilter = new GlobPathFilter(Glob(glob))

  /**
   * Converts a [[Glob]] to a [[PathFilter]]. The [[PathFilter.accept]] method will ignore the
   * [[FileAttributes]] parameter and return true if the `path` parameter is accepted by the
   * input glob.
   * @param glob the glob string to convert to a filter, e.g. "**<code>/</code>*.scala"
   * @return the [[PathFilter]] corresponding to the parsed [[Glob]]. May throw an exception if the glob string can not be parsed into a
   *         [[Glob]].
   */
  implicit def globToPathFilter(glob: Glob): PathFilter = new GlobPathFilter(glob)
  private class GlobPathFilter(private val glob: Glob) extends PathFilter {
    override def accept(path: Path, attributes: FileAttributes): Boolean = glob.matches(path)
    override def toString: String = glob.toString
    override def equals(obj: Any): Boolean = obj match {
      case that: GlobPathFilter => this.glob == that.glob
      case _                    => false
    }
    override def hashCode: Int = glob.##
  }
}
object PathFilter extends LowPriorityPathFilter {

  /**
   * Returns a PathFilter that accepts any path for which there exists a [[Glob]] that matches
   * the path
   * @param globs the glob patterns
   * @return a PathFilter that accepts any path matching one or more input [[Glob]]s.
   */
  def apply(globs: Glob*): PathFilter = if (globs.isEmpty) NoPass else new GlobPathFilter(globs)

  private[file] trait PathFilterExtensions extends Any {

    /**
     * Combines this filter with a [[sbt.nio.file.PathFilter]] to produce a combined filter
     * that returns true only if both filters accept the path.
     * @param other the other [[sbt.nio.file.PathFilter]]
     * @return the [[sbt.nio.file.PathFilter]] representing both filters combined by the `&&`
     *         operation
     */
    def &&(other: PathFilter): PathFilter

    /**
     * Combines this filter with a [[sbt.nio.file.PathFilter]] to produce a combined filter
     * that returns true either if this or the other [[sbt.nio.file.PathFilter]] accept the path.
     * @param other the other [[sbt.nio.file.PathFilter]]
     * @return the [[sbt.nio.file.PathFilter]] representing both filters combined by the `&&`
     *         operation
     */
    def ||(other: PathFilter): PathFilter

    /**
     * Creates a new [[sbt.nio.file.PathFilter]] what accepts a `(Path, FileAttributes)` pair only
     * if this filter does not accept it.
     * @return the negated [[sbt.nio.file.PathFilter]] corresponding to this filter
     */
    def unary_! : PathFilter
  }

  /**
   * Provides extension methods for combining or negating [[PathFilter]] instances or
   * or other filter types that can be safely converted (see [[sbt.io.DirectoryFilter]]
   * and [[sbt.io.HiddenFileFilter]]).
   */
  implicit class Ops(val pathFilter: PathFilter) extends AnyVal with PathFilterExtensions {
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
    def unary_! : PathFilter = pathFilter match {
      case AllPass     => NoPass
      case NoPass      => AllPass
      case IsHidden    => IsNotHidden
      case IsNotHidden => IsHidden
      case pf          => new NotPathFilter(pf)
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
  def fromFileFilter(fileFilter: FileFilter): PathFilter = fileFilter match {
    case sbt.io.HiddenFileFilter    => IsHidden
    case sbt.io.NotHiddenFileFilter => IsNotHidden
    case sbt.io.AllPassFilter       => AllPass
    case sbt.io.NothingFilter       => NoPass
    case sbt.io.DirectoryFilter     => IsDirectory
    case sbt.io.RegularFileFilter   => IsRegularFile
    case pf: PathFilter             => pf
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
private[sbt] case object IsDirectory extends PathFilter {

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
private[sbt] case object IsRegularFile extends PathFilter {

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
private[sbt] case object IsHidden extends PathFilter {

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
private[sbt] case object IsNotHidden extends PathFilter {
  override def accept(path: Path, attributes: FileAttributes): Boolean = !Files.isHidden(path)
}

private[sbt] case object AllPass extends PathFilter {

  /**
   * Always returns true.
   * @param path the path name
   * @param attributes the file attributes corresponding to `path`
   * @return true.
   */
  override def accept(path: Path, attributes: FileAttributes): Boolean = true
}

private[sbt] case object NoPass extends PathFilter {

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
