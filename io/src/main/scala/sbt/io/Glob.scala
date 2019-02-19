/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.io

import java.io.File
import java.nio.file.{ Path => NioPath }

import sbt.io

/**
 * Represents a filtered subtree of the file system.
 */
trait Glob {

  /**
   * The root of the file system subtree.
   */
  def base: NioPath

  /**
   * The maximum depth of elements to traverse. A depth of -1 implies that this glob applies only
   * to the root specified by [[base]]. A depth of zero implies that only immediate children of
   * the root are included in this glob. For positive depth, files may be included so long as their
   * pathname, relativized with respect to the base, has no more than `depth + 1` components.
   * @return the maximum depth.
   */
  def depth: Int

  /**
   * The filter to apply to elements found in the file system subtree.
   * @return the filter.
   */
  def filter: NioPath => Boolean

}
sealed trait GlobBuilder[G] extends Any {
  def /(component: String): G
  def \(component: String): G
  def glob(filter: FileFilter): G
  def *(filter: FileFilter): G
  def globRecursive(filter: FileFilter): G
  def allPaths: G
  def **(filter: FileFilter): G
}
sealed trait ToGlob extends Any {
  def toGlob: Glob
}
object Glob {
  private[sbt] implicit class ConvertedFileFilter(val f: FileFilter) extends (NioPath => Boolean) {
    override def apply(path: NioPath): Boolean = f.accept(path.toFile)
    override def equals(o: Any): Boolean = o match {
      case that: ConvertedFileFilter => this.f == that.f
      case _                         => false
    }
    override def hashCode: Int = f.hashCode
    override def toString: String = s"ConvertedFileFilter($f)"
  }
  private implicit class PathOps(val p: NioPath) extends AnyVal {
    def abs: NioPath = if (p.isAbsolute) p else p.toAbsolutePath
  }
  def apply(base: File, depth: Int, filter: FileFilter): Glob =
    new GlobImpl(base.toPath.abs, depth, filter)
  def apply(base: NioPath, depth: Int, filter: NioPath => Boolean): Glob =
    new GlobImpl(base.abs, depth, filter)
  private class GlobImpl(val base: NioPath, val depth: Int, val filter: NioPath => Boolean)
      extends Glob {
    override def toString: String =
      s"Glob(\n  base = $base,\n  filter = $filter,\n  depth = $depth\n)"
    override def equals(o: Any): Boolean = o match {
      case that: Glob =>
        this.base == that.base && this.depth == that.depth && this.filter == that.filter
      case _ => false
    }
    override def hashCode: Int = (((base.hashCode * 31) ^ filter.hashCode) * 31) ^ depth
  }
  private[sbt] trait Builder[T] extends Any with GlobBuilder[Glob] with ToGlob {
    def repr: T
    def converter: T => NioPath
    def /(component: String): Glob = {
      val base = converter(repr).resolve(component)
      Glob(base, -1, new ExactFileFilter(base.toFile))
    }
    def \(component: String): Glob = this / component
    def glob(filter: FileFilter): Glob = Glob(converter(repr), 0, filter)
    def *(filter: FileFilter): Glob = glob(filter)
    def globRecursive(filter: FileFilter): Glob = Glob(converter(repr), Int.MaxValue, filter)
    def allPaths: Glob = globRecursive(AllPassFilter)
    def **(filter: FileFilter): Glob = globRecursive(filter)
    def toGlob: Glob = {
      val base = converter(repr)
      Glob(base, -1, new ExactFileFilter(base.toFile))
    }
  }
  final class FileBuilder(val file: File) extends AnyVal with Builder[File] {
    override def repr: File = file
    override def converter: File => NioPath = (_: File).toPath
  }
  final class PathBuilder(val path: NioPath) extends AnyVal with Builder[NioPath] {
    override def repr: NioPath = path
    override def converter: NioPath => NioPath = identity
  }
  implicit class GlobOps(val glob: Glob) extends AnyVal {
    def withBase(base: File): Glob = new GlobImpl(base.toPath, glob.depth, glob.filter)
    def withBase(base: NioPath): Glob = new GlobImpl(base, glob.depth, glob.filter)
    def withFilter(filter: FileFilter): Glob = new GlobImpl(glob.base, glob.depth, filter)
    def withDepth(depth: Int): Glob = new GlobImpl(glob.base, depth, glob.filter)
    def withRecursive(recursive: Boolean): Glob =
      new GlobImpl(glob.base, if (recursive) Int.MaxValue else 0, glob.filter)
    def toFileFilter: FileFilter = toFileFilter(acceptBase = true)
    def toFileFilter(acceptBase: Boolean): FileFilter = new GlobAsFilter(glob, acceptBase)
  }
  implicit def toPathFinder(glob: Glob): PathFinder = new io.PathFinder.GlobPathFinder(glob)
  implicit object ordering extends Ordering[Glob] {
    override def compare(left: Glob, right: Glob): Int = left.base.compareTo(right.base) match {
      // We want greater depth to come first because when we are using a Seq[Glob] to
      // register with the file system cache, it is more efficient to register the broadest glob
      // first so that we don't have to list the base directory multiple times.
      case 0 =>
        // If we inline -left.depth.compareTo(right.depth), scala 2.10 incorrectly reports
        // an implicit numeric widening error. This could be inlined if we drop 2.10 support.
        val leftDepth: Int = left.depth
        -leftDepth.compareTo(right.depth)
      case i => i
    }
  }

  /**
   * Provides a [[FileFilter]] given a [[Glob]].
   * @param glob the glob to validate
   * @param acceptBase toggles whether or not we should accept the base path even when the depth is
   *                   greater than or equal to zero.
   */
  final class GlobAsFilter(private val glob: Glob, private val acceptBase: Boolean)
      extends FileFilter {
    override def accept(pathname: File): Boolean = {
      val path = pathname.toPath
      val globPath = glob.base
      if (path.startsWith(globPath)) {
        if (path == globPath) {
          (acceptBase || glob.depth == -1) && glob.filter(path)
        } else {
          val nameCount = globPath.relativize(path).getNameCount - 1
          nameCount <= glob.depth && glob.filter(path)
        }
      } else {
        false
      }
    }
    override def toString: String = s"GlobAsFilter($glob)"
    override def equals(o: Any): Boolean = o match {
      case that: GlobAsFilter => this.acceptBase == that.acceptBase && this.glob == that.glob
      case _                  => false
    }
  }

  /**
   * Provides extension methods for converting a Traversable[Glob] into a file filter.
   * @param t the collection of [[Glob]]s
   * @tparam T the generic collection type
   */
  private[sbt] implicit class TraversableGlobOps[T <: Traversable[Glob]](val t: T) extends AnyVal {

    /**
     * Returns a [[FileFilter]] that accepts a file if any glob in the collection accepts the file.
     * The filter will accept the base file of each [[Glob]] in the collection.

     * @return the [[FileFilter]].
     */
    def toFileFilter: FileFilter = new TraversableGlobOps.Filter(t, true)

    /**
     * Returns a [[FileFilter]] that accepts a file if any glob in the collection accepts the file.
     *
     * @param acceptBase toggles whether or not the base file of a [[Glob]] should be accepted by the
     *                   filter
     * @return the [[FileFilter]].
     */
    def toFileFilter(acceptBase: Boolean): FileFilter = new TraversableGlobOps.Filter(t, acceptBase)

  }
  private[sbt] object TraversableGlobOps {
    private class Filter[T <: Traversable[Glob]](private val t: T, private val acceptBase: Boolean)
        extends FileFilter {
      private[this] val filters = t.map(_.toFileFilter(acceptBase))
      override def accept(pathname: File): Boolean = filters.exists(_.accept(pathname))
      override def equals(o: Any): Boolean = o match {
        case that: Filter[_] => this.t == that.t && this.acceptBase == that.acceptBase
        case _               => false
      }
      override def hashCode: Int = (t.hashCode * 31) ^ acceptBase.hashCode
      override def toString: String =
        s"TraversableGlobFilter(filters = $t, acceptBase = $acceptBase)"
    }
  }
}
