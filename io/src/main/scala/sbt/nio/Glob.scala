/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio

import java.io.{ File, IOException }
import java.nio.file._
import java.util

import sbt.io.{ FileFilter, PathFinder, SimpleFileFilter }

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Represents a filtered subtree of the file system.
 */
sealed trait Glob {

  /**
   * The root of the file system subtree.
   */
  def base: Path

  /**
   * Controls which paths should be considered part of the glob based on the number of components
   * in the path when it is relativized with respect to the base path. The boundaries are inclusive.
   * A range of `(0, 0)` implies that only the base path is accepted. A range of `(0, 1)` implies
   * that the base path and its immediate children are accepted but no children of subdirectories
   * are included. A range of `(1, Int.MaxValue)` implies that all children of the base path are
   * accepted and so on.
   *
   * @return the range of relative path name lengths to accepts.
   */
  def range: (Int, Int)

  /**
   * The filter to apply to elements found in the file system subtree.
   * @return the filter.
   */
  def pathFilter: PathFilter

}
private[sbt] sealed trait GlobBuilder[G] extends Any {
  def /(component: String): G
  def \(component: String): G
  def glob(filter: FileFilter): G
  def *(filter: FileFilter): G
  def globRecursive(filter: FileFilter): G
  def allPaths: G
  def **(filter: FileFilter): G
}
private[sbt] sealed trait ToGlob extends Any {
  def toGlob: Glob
}
object Glob extends LowPriorityGlobOps {

  /**
   * Converts a string to a [[Glob]]. The string may contain wild cards. For example, when the
   * input string is '/foo/bar', the returned glob accepts only the exact path '/foo/bar'. When
   * the input string is '/foo/bar/src/main/scala/**/*.scala', the returned glob will accept
   * all files with the '.scala' extension that are children of '/foo/bar/src/main/scala'.
   * @param path the path to convert to a Glob
   * @return a Glob
   */
  implicit def stringToGlob(path: String): Glob = Glob.parse(path)

  private final val singlePathRange = (0, 0)
  implicit def pathToGlob(path: Path): Glob = new GlobImpl(path, singlePathRange, AllPass)
  implicit def fileToGlob(file: File): Glob = new GlobImpl(file.toPath, singlePathRange, AllPass)

  object GlobOps {
    final implicit class PathOps(path: Path) extends GlobOps {
      override val glob: Glob = pathToGlob(path)
    }
  }

  /**
   * Provides extension methods for a [[Glob]]. These allow us to extend the glob from its
   * base path using various `/` methods.
   */
  sealed trait GlobOps extends Any {
    def glob: Glob
    private[sbt] def toFileFilter: sbt.io.FileFilter =
      new SimpleFileFilter(file => filter.accept(file.toPath))

    /**
     * Check if a path is included in the range of a glob. For example, if the glob is
     * `/foo/bar/ * / *.scala` then `/foo/bar/src/Foo.scala` is in the range, but neither
     * `foo/bar/Foo.scala` nor `foo/bar/src/main/scala/Foo.scala` is in range
     * @param path the path to check
     * @return true if the path is in range
     */
    def inRange(path: Path): Boolean = {
      val base = glob.base
      if (path.startsWith(base)) {
        val (min, max) = glob.range
        if (path == base) {
          min <= 0
        } else {
          val nameCount = base.relativize(path).getNameCount
          nameCount >= min && nameCount <= max
        }
      } else {
        false
      }
    }

    /**
     * Returns a filter that checks both that the path is in the glob's range and that it matches
     * the glob's [[PathFilter]].
     * @return the combined [[PathFilter]]
     */
    def filter: PathFilter = (path: Path) => inRange(path) && glob.pathFilter.accept(path)
    private[this] def incrementRange(recursive: Boolean, strict: Boolean): (Int, Int) = {
      lazy val msg = s"Couldn't increment range for $glob."
      if (strict && glob.range._2 == Int.MaxValue) throw new IllegalArgumentException(msg)
      glob.range match {
        case (min, _) if recursive && glob.pathFilter == AllPass =>
          (min + 1, Int.MaxValue)
        case r @ (min, max) if !recursive && glob.pathFilter == AllPass =>
          if (max == Int.MaxValue) r else (min + 1, max + 1)
        case _ => throw new IllegalArgumentException(msg)
      }
    }

    /**
     * Add a path name filter to the glob. This will also implicitly increase the range of the
     * glob by one. For example, when the current glob is `/foo/bar` and the input filter is
     * the extension filter "*.txt", then the new glob will accept all immediate globs of
     * `/foo/bar` with the extension *.txt. When the current glob is `/foo/bar/ * / **` and the
     * filter is the extension filter "*.txt", then the new glob will accept all files with
     * the extension ".txt" that are descendants of `/foo/bar` but not directly in `/foo/bar`.
     *
     * @param pathNameFilter the name filter to apply to the children of the previous glob
     * @return the new filtered glob.
     */
    def /(pathNameFilter: PathNameFilter): Glob =
      Glob(glob.base, incrementRange(recursive = false, strict = false), filter = pathNameFilter)

    /**
     * Extend the glob with an arbitrary string. This can be some arbitrary syntax. For example,
     * if the current glob is `/foo/bar` and the input is '/baz/**/foo*' then the returned glob
     * will accept all files that start with the prefix 'foo' that are children of `/foo/bar/baz`.
     * @param globPath the path (that may contain wildcard '*' characters).
     * @return the transformed glob
     */
    def /(globPath: String): Glob = {
      if (glob.pathFilter != AllPass)
        throw new IllegalArgumentException(s"Couldn't append $globPath to $glob")
      val res = parse(glob = s"$glob/$globPath")
      res
    }
  }
  implicit class GlobOpsImpl(override val glob: Glob) extends AnyVal with GlobOps
  implicit object ordering extends Ordering[Glob] {
    private[this] val pairOrdering: Ordering[(Int, Int)] = Ordering[(Int, Int)]
    override def compare(left: Glob, right: Glob): Int = left.base.compareTo(right.base) match {
      // We want greater depth to come first because when we are using a Seq[Glob] to
      // register with the file system cache, it is more efficient to register the broadest glob
      // first so that we don't have to list the base directory multiple times.
      case 0 => pairOrdering.compare(right.range, left.range)
      case i => i
    }
  }

  private[nio] def parse(glob: String): Glob = {
    val array = glob.toCharArray
    val stringBuilder = new StringBuilder
    val components = new ArrayBuffer[String]
    var firstStarIndex = -1
    @tailrec def fillComponents(index: Int): Unit = {
      if (index < array.length) {
        array(index) match {
          case '/' | '\\' =>
            components += stringBuilder.toString.trim
            stringBuilder.clear()
            fillComponents(index + 1)
          case c =>
            if (c == '*' && firstStarIndex == -1) firstStarIndex = components.size
            stringBuilder += c
            fillComponents(index + 1)
        }
      } else if (stringBuilder.nonEmpty) {
        components += stringBuilder.toString.trim
      }
    }
    fillComponents(index = 0)
    val pathNameCount = if (firstStarIndex == -1) components.size else firstStarIndex
    val (pathPart, tail) = components.splitAt(pathNameCount)
    val pathString = pathPart.mkString(File.separator)
    val path = Paths.get(pathString)
    val filterIndex = tail.indexWhere(s => s != "*" && s != "**")
    val (rangeParts, filterParts) =
      if (filterIndex == -1) (tail, Nil) else tail.splitAt(filterIndex)

    val range =
      if (rangeParts.isEmpty && filterParts.nonEmpty) (1, 1)
      else {
        val base = rangeParts.foldLeft(if (pathString.isEmpty) (-1, -1) else (0, 0)) {
          case ((min, max), part) if part == "*" && max != Int.MaxValue  => (min + 1, max + 1)
          case ((min, max), part) if part == "**" && max != Int.MaxValue => (min + 1, Int.MaxValue)
          case ((_, max), _) if max == Int.MaxValue =>
            throw new IllegalArgumentException(
              s"Range cannot be extended for recursive glob: $glob")
        }
        if (filterParts.isEmpty) base
        else
          base match {
            case (min, max) if max != Int.MaxValue => (min + 1, max + 1)
            case _                                 => base
          }
      }
    val filter =
      if (filterParts.isEmpty) AllPass
      else if (filterParts.size > 1)
        throw new IllegalArgumentException(
          s"Couldn't construct glob instance for argument $glob with multiple trailing paths after a range parameter ('*' or '**').")
      else PathNameFilter(filterParts.head)

    Glob(path, range, filter)
  }
  private[nio] class ConvertedFileFilter(val f: FileFilter) extends PathNameFilter {
    override def accept(path: Path): Boolean = f.accept(path.toFile)
    override def accept(name: String): Boolean = f match {
      case nf: sbt.io.NameFilter => nf.accept(name)
      case _                     => false
    }
    override def equals(o: Any): Boolean = o match {
      case that: ConvertedFileFilter => this.f == that.f
      case _                         => false
    }
    override def hashCode: Int = f.hashCode
    override def toString: String = s"ConvertedFileFilter($f)"
  }
  private[sbt] object ConvertedFileFilter {
    def applyName(nameFilter: sbt.io.NameFilter): PathNameFilter = nameFilter match {
      case af: sbt.io.AndNameFilter   => new AndNameFilter(applyName(af.left), applyName(af.right))
      case of: sbt.io.OrNameFilter    => new OrNameFilter(applyName(of.left), applyName(of.right))
      case ef: sbt.io.ExtensionFilter => new ExtensionFilter(ef.extensions: _*)
      case ef: sbt.io.ExactFilter     => new ExactNameFilter(ef.matchName)
      case nn: sbt.io.NotNameFilter   => new NotNameFilter(applyName(nn.fileFilter))
      case pf: sbt.io.PrefixFilter    => new SplitFilter(pf.prefix, suffix = "")
      case sf: sbt.io.SuffixFilter    => new SplitFilter(prefix = "", sf.suffix)
      case nf: sbt.io.NameFilter      => new ConvertedFileFilter(nf)
    }
    def apply(fileFilter: FileFilter): PathFilter = fileFilter match {
      case sbt.io.AllPassFilter       => AllPass
      case sbt.io.NothingFilter       => NoPass
      case af: sbt.io.AndFilter       => new AndFilter(apply(af.left), apply(af.right))
      case of: sbt.io.OrFilter        => new OrFilter(apply(of.left), apply(of.right))
      case nf: sbt.io.NameFilter      => applyName(nf)
      case nf: sbt.io.NotFilter       => new NotFilter(apply(nf.fileFilter))
      case ef: sbt.io.ExactFileFilter => new ExactPathFilter(ef.file.toPath)
      case filter: sbt.io.FileFilter  => new ConvertedFileFilter(filter)
    }
  }
  private[this] def abs(path: Path): Path = if (path.isAbsolute) path else path.toAbsolutePath
  private[sbt] def apply(base: Path, range: (Int, Int), filter: PathFilter): Glob =
    new GlobImpl(abs(base), range, filter)
  def apply(base: Path): Glob = new GlobImpl(abs(base), singlePathRange, AllPass)
  def apply(base: Path, range: (Int, Int), filter: PathNameFilter): Glob =
    new GlobImpl(abs(base), range, filter)
  private def show(glob: Glob): String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append(glob.base)
    glob.range match {
      case `singlePathRange` =>
      case (starCount, max) =>
        (1 until starCount).foreach(_ => stringBuilder.append(File.separator).append('*'))
        if (max == Int.MaxValue) stringBuilder.append(File.separator).append("**")
    }
    glob.pathFilter match {
      case AllPass =>
        glob.range match {
          case (_, max) if max == Int.MaxValue =>
          case (min, _)                        => if (min > 0) stringBuilder.append(File.separator).append('*')
        }
      case f => stringBuilder.append(File.separator).append(f)
    }
    stringBuilder.toString
  }
  private[this] class GlobImpl(override val base: Path,
                               override val range: (Int, Int),
                               override val pathFilter: PathFilter)
      extends Glob {
    override def toString: String = show(glob = this)
    override def equals(o: Any): Boolean = o match {
      case that: GlobImpl =>
        this.base == that.base && this.range == that.range && this.pathFilter == that.pathFilter
      case _ => false
    }
    override def hashCode: Int =
      (((base.hashCode * 31) ^ pathFilter.hashCode) * 31) ^ range.hashCode
  }
  private[sbt] trait Builder[T] extends Any with GlobBuilder[Glob] with ToGlob {
    def repr: T
    def converter: T => Path
    def /(component: String): Glob = {
      val base = converter(repr).resolve(component)
      Glob(base.resolve(component), singlePathRange, AllPass)
    }
    def \(component: String): Glob = this / component
    def glob(filter: FileFilter): Glob =
      new GlobImpl(converter(repr), (1, 1), ConvertedFileFilter(filter))
    def *(filter: FileFilter): Glob = glob(filter)
    def globRecursive(filter: FileFilter): Glob =
      new GlobImpl(converter(repr), (1, Int.MaxValue), ConvertedFileFilter(filter))
    def allPaths: Glob = globRecursive(sbt.io.AllPassFilter)
    def **(filter: FileFilter): Glob = globRecursive(filter)
    def toGlob: Glob = {
      val base = converter(repr)
      new GlobImpl(base, singlePathRange, AllPass)
    }
  }
  private[sbt] final class FileBuilder(val file: File) extends AnyVal with Builder[File] {
    override def repr: File = file
    override def converter: File => Path = (_: File).toPath
  }
  private[sbt] final class PathBuilder(val path: Path) extends AnyVal with Builder[Path] {
    override def repr: Path = path
    override def converter: Path => Path = identity
  }

  private[sbt] def all(globs: Traversable[Glob],
                       view: FileTreeView.Nio[FileAttributes]): Seq[(Path, FileAttributes)] =
    all(globs, view, (_, _) => true)
  private[sbt] def all(globs: Traversable[Glob],
                       view: FileTreeView.Nio[FileAttributes],
                       filter: (Path, FileAttributes) => Boolean): Seq[(Path, FileAttributes)] =
    iterator(globs, view, filter).toVector

  private[sbt] def iterator(
      globs: Traversable[Glob],
      view: FileTreeView.Nio[FileAttributes]): Iterator[(Path, FileAttributes)] =
    iterator(globs, view, (_, _) => true)
  private[sbt] def iterator(
      globs: Traversable[Glob],
      view: FileTreeView.Nio[FileAttributes],
      filter: (Path, FileAttributes) => Boolean): Iterator[(Path, FileAttributes)] = {
    val sorted = globs.toSeq.sorted
    val needListDirectory: Path => Boolean = (path: Path) => {
      val filters = sorted.map(g => Glob(g.base, (0, g.range._2), AllPass).filter)
      val a = path.resolve("a")
      filters.exists(_.accept(a))
    }
    val visited = new util.HashSet[Path]
    val pathFilter: PathFilter = {
      val filters = sorted.map(_.filter)
      path: Path =>
        filters.exists(_.accept(path))
    }
    val totalFilter: (Path, FileAttributes) => Boolean = { (path, attributes) =>
      pathFilter.accept(path) && filter(path, attributes)
    }
    val remainingGlobs = new util.LinkedList[Glob]()
    sorted.foreach(remainingGlobs.add)
    val remainingPaths = new util.LinkedList[Path]()
    val iterator: Iterator[(Path, FileAttributes)] = new Iterator[(Path, FileAttributes)] {
      private[this] val buffer = new util.LinkedList[(Path, FileAttributes)]
      private[this] val maybeAdd: ((Path, FileAttributes)) => Unit = {
        case pair @ (path, attributes) =>
          if (totalFilter(path, attributes)) buffer.add(pair)
          ()
      }
      @tailrec
      private def fillBuffer(): Unit = {
        remainingPaths.poll match {
          case null =>
            remainingGlobs.poll() match {
              case null =>
              case g =>
                remainingPaths.add(g.base)
                fillBuffer()
            }
          case path if !visited.contains(path) =>
            visited.add(path)
            path.getParent match {
              case null =>
              case p =>
                if (!visited.contains(p) && pathFilter.accept(path))
                  FileAttributes(path).foreach(a => maybeAdd(path -> a))
            }
            try {
              view.list(path) foreach {
                case pair @ (p, attributes) if attributes.isDirectory =>
                  if (needListDirectory(p)) remainingPaths.add(p)
                  maybeAdd(pair)
                case pair => maybeAdd(pair)
              }
            } catch {
              case _: IOException =>
            }
            if (buffer.isEmpty) fillBuffer()
          case _ =>
        }
      }
      override def hasNext: Boolean = !buffer.isEmpty
      override def next(): (Path, FileAttributes) = {
        val res = buffer.poll()
        if (buffer.isEmpty) {
          fillBuffer()
        }
        res
      }
      fillBuffer()
    }
    iterator
  }
}
private[nio] trait LowPriorityGlobOps {
  implicit def toPathFinder(glob: Glob): PathFinder = new PathFinder.GlobPathFinder(glob)
}
