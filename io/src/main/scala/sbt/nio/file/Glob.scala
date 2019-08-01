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

import java.io.File
import java.nio.file.{ FileSystems, Path, Paths }
import java.util

import sbt.io._
import sbt.nio.file.RelativeGlob.{ PathComponent, SingleComponentMatcher }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.Properties

/**
 * Represents a query for a path on the file system. Instances of `Glob` can be combined together
 * to build a more complicated query. Similar to the path name components of a `Path`, a `Glob` has
 * components and matching is done from left to right component-wise. Each component is matched
 * using a
 * [[https://docs.oracle.com/javase/8/docs/api/java/nio/file/FileSystem.html#getPathMatcher-java.lang.String-
 * PathMatcher]].
 * For example,
 * {{{
 *   val baseGlob = Glob("/foo/bar")
 *   baseGlob.matches(Paths.get("/foo/bar")) // true
 *   baseGlob.matches(Paths.get("/foo")) // false
 *   baseGlob.matches(Paths.get("/foo/bar/baz")) // false
 *
 *   val children = baseGlob / AnyPath
 *   children.matches(Paths.get("/foo/bar")) // false
 *   children.matches(Paths.get("/foo/bar/baz")) // true
 *   children.matches(Paths.get("/foo/bar/baz/buzz")) false
 *
 *   val secondGenerationChildren = children / AnyPath
 *   secondGeneration.matches(Paths.get("/foo/bar/baz")) // false
 *   secondGeneration.matches(Paths.get("/foo/bar/baz/buzz")) // true
 *   secondGeneration.matches(Paths.get("/foo/bar/baz/buzz/fizz")) // false
 * }}}
 * where [[AnyPath]] matches any path name.
 *
 * Globs can be recursive and are constructed using the [[RecursiveGlob]]:
 * {{{
 *   val baseGlob = Glob("/foo/bar")
 *   val allDescendants = baseGlob / RecursiveGlob
 *   allDescendants.matches(Paths.get("/foo/bar")) // false
 *   allDescendants.matches(Paths.get("/foo/bar/baz/buzz.txt")) // true
 * }}}
 *
 * They can also filter on the basis file names:
 * {{{
 *   val baseGlob = Glob("/foo/bar")
 *   val allScalaSources = baseGlob / RecursiveGlob / "*.scala"
 *   allScalaSources.matches(Paths.get("/foo/bar/Foo.scala")) // true
 *   allScalaSources.matches(Paths.get("/foo/bar/baz/fizz/buzz/Bar.scala")) // true
 *   allScalaSources.matches(Paths.get("/foo/bar/baz/fizz/buzz/Buzz.java")) // false
 *
 *   val allScalaAndJavaSources = baseGlob / RecursiveGlob / "*.{java,scala}"
 *   allScalaAndJavaSources.matches(Paths.get("/foo/bar/baz/fizz/buzz/Bar.scala")) // true
 *   allScalaAndJavaSources.matches(Paths.get("/foo/bar/baz/fizz/buzz/Buzz.java")) // true
 * }}}
 */
sealed trait Glob {

  /**
   * Indicates whether a path matches the pattern specified by this [[Glob]].
   *
   * @param path the path to match
   * @return true it the path matches.
   */
  def matches(path: Path): Boolean
}

object Glob {
  private[this] def comp[T](left: T, right: T)(implicit ordering: Ordering[T]): Int =
    ordering.compare(left, right)

  /**
   * Construct a single file [[Glob]]. The resulting [[Glob]] will only match the input file.
   * @param file the single path to match.
   * @return a Glob whose [[Glob.matches]] method only accepts the input path.
   */
  def apply(file: File): Glob = if (file.isAbsolute) Root(file.toPath) else apply(file.toString)

  /**
   * Construct a Glob with a file prefix and relativeGlob glob string. For example:
   * {{{
   *   val glob = Glob(file("foo/bar"), "*.txt") // matches all txt files in foo/bar
   *   val recursiveGlob = Glob(file("foo/bar"), "**<code>/</code>*.txt") // recursively matches all text files in foo/bar
   * }}}
   * @param file the prefix of the glob pattern. This can be an absolute or relativeGlob file.
   * @param relative the relativeGlob glob string
   * @return a Glob that matches the file prefix with
   */
  def apply(file: File, relative: String): Glob = Glob(file, RelativeGlob.parse(relative))

  /**
   * Construct a [[Glob]] with a file prefix and a [[RelativeGlob]] suffix. In order for a path to
   * match the output [[Glob]], it must start with `file` and its relative path with respect to
   * `file` must match the `relativeGlob`. For example:
   * {{{
   *   val txtFiles = Glob(file("foo/bar"), * / "*.txt") // matches all text files in any subdirectory of foo/bar
   *   val allChildren = Glob(file("/foo/bar"), **) // matches all descendants of /foo/bar
   *   val scalaSources = Glob(file("src/main/scala", ** / "*.scala")) // matches scala source file
   * }}}
   *
   * @param file the prefix of the glob pattern. This can be an absolute or relativeGlob file.
   * @param relativeGlob the [[RelativeGlob]] to append to the file prefix
   * @return a Glob that matches a path with the same prefix as `file` and whose suffix matches
   *         `relativeGlob`.
   */
  def apply(file: File, relativeGlob: RelativeGlob): Glob = Glob(file.toPath) / relativeGlob

  /**
   * Construct a single path Glob. The resulting glob
   * @param path the single path to match.
   * @return a Glob whose [[Glob.matches]] method only accepts the input path.
   */
  def apply(path: Path): Glob = if (path.isAbsolute) Root(path) else apply(path.toString)

  /**
   * Construct a Glob with a path prefix and a `RelativeGlob`. For example:
   * {{{
   *   val txtFiles = Glob(Paths.get("foo/bar"), * / "*.txt") // matches all text files in any subdirectory of foo/bar
   *   val allChildren = Glob(Paths.get("/foo/bar"), **) // matches all descendants of /foo/bar
   *   val scalaSources = Glob(Paths.get("src/main/scala", ** / "*.scala")) // matches scala source file
   * }}}
   * @param path the prefix of the glob pattern. This can be an absolute or relativeGlob file.
   * @param relativeGlob the [[RelativeGlob]] to append to the file prefix
   * @return a Glob that matches the file prefix with
   */
  def apply(path: Path, relativeGlob: RelativeGlob): Glob = Glob(path) / relativeGlob

  /**
   * Construct a Glob with a file prefix and relativeGlob glob string. For example:
   * {{{
   *   val glob = Glob(file("foo/bar"), "*.txt") // matches all text files in foo/bar
   * }}}
   * @param path the prefix of the glob pattern. This can be an absolute or relativeGlob file.
   * @param relative the relativeGlob glob string
   * @return a Glob that matches the file prefix with
   */
  def apply(path: Path, relative: String): Glob = Glob(path) / relative

  /**
   * Construct a glob from a string. It can represent a full path. Any `/` characters in a pathname
   * are automatically converted to `\\` on windows.
   * {{{
   *   val absoluteGlob = Glob("/foo/bar<code>/</code>**<code>/</code>*.txt")
   *   val relativeGlob = Glob("*<code>/</code>*.txt")
   * }}}
   * @param glob the string to convert to a glob
   * @return a parsed `Glob` for the input. May throw if the input cannot be parsed into a `Glob`.
   */
  def apply(glob: String): Glob = {
    val parts = splitter(glob)
    @tailrec def fullGlob(path: Path, rest: List[String]): Glob = {
      rest match {
        case component :: tail if !hasMeta(component) => fullGlob(path.resolve(component), tail)
        case Nil                                      => Root(path)
        case _                                        => Pattern(path, RelativeGlob(rest: _*))
      }
    }
    parts match {
      case h :: rest if !hasMeta(h) =>
        val base = if (isWin) {
          if (h.endsWith(":")) Paths.get(h + File.separator) else Paths.get(h)
        } else {
          if (h.isEmpty) Paths.get("/") else Paths.get(h)
        }
        base match {
          case p if p.isAbsolute => fullGlob(p, rest)
          case _                 => RelativeGlob(parts: _*)
        }
      case _ => RelativeGlob(parts: _*)
    }
  }

  /**
   * Provides a default ordering for glob. Globs with a path prefix precede relative globs and are
   * sorted by their path prefix.
   */
  implicit object ordering extends Ordering[Glob] {
    override def compare(left: Glob, right: Glob): Int = left match {
      case Pattern(leftRoot, leftRelative) =>
        right match {
          case Pattern(rightRoot, rightRelative) =>
            leftRoot.compareTo(rightRoot) match {
              case 0 => compare(leftRelative, rightRelative)
              case i => i
            }
          case Root(rightRoot) =>
            leftRoot.compareTo(rightRoot) match {
              case 0 => -1
              case i => i
            }
          case FullFileGlob(base, _, _) =>
            leftRoot.compareTo(base) match {
              case 0 => -1
              case i => i
            }
          case _: RelativeGlob => -1
        }
      case l: RelativeGlob =>
        right match {
          case r: RelativeGlob => comp(l.matchers, r.matchers)
          case _: Pattern      => 1
          case _: Root         => 1
          case _: FullFileGlob => 1
        }
      case Root(leftRoot) =>
        right match {
          case Root(rightRoot) => leftRoot.compareTo(rightRoot)
          case _               => -compare(right, left)
        }
      case FullFileGlob(leftBase, _, _) =>
        right match {
          case FullFileGlob(rightBase, _, _) => leftBase.compareTo(rightBase)
          case _                             => -compare(right, left)
        }
    }
  }
  private object Root {
    implicit val ordering: Ordering[Root] = Ordering.by(_.root)
  }
  private final case class Root(root: Path) extends Glob {
    require(root.isAbsolute, s"Tried to construct absolute glob from relative path $root")
    override def matches(path: Path): Boolean = root == path
    override def toString: String = root.toString
    override def equals(o: Any): Boolean = o match {
      case that: Root => this.root == that.root
      case _          => false
    }
    override def hashCode: Int = root.hashCode
  }

  private[sbt] final case class FullFileGlob(base: Path, recursive: Boolean, filter: FileFilter)
      extends Glob {
    override def matches(path: Path): Boolean = {
      path.startsWith(base) && {
        if (recursive) filter.accept(path.toFile)
        else path.getParent == base && filter.accept(path.toFile)
      }
    }
  }

  private[nio] object Pattern {
    implicit val ordering: Ordering[Pattern] = Ordering.by(p => (p.root, p.relative))
  }
  private[nio] final case class Pattern(root: Path, relative: RelativeGlob) extends Glob {
    override def matches(path: Path): Boolean =
      path.startsWith(root) && ((path != root) && relative.matches(root.relativize(path)))
    override def equals(o: Any): Boolean = o match {
      case that: Pattern =>
        this.root == that.root && this.relative == that.relative
      case _ => false
    }
    override def hashCode: Int = (root.hashCode * 31) ^ relative.hashCode
    override def toString: String = s"$root${File.separatorChar}$relative"
  }

  private[sbt] sealed trait RelativeGlobViewOption
  private[sbt] object RelativeGlobViewOption {
    private[sbt] val propName = "sbt.io.implicit.relative.glob.conversion"
    implicit val default: RelativeGlobViewOption = System.getProperty(propName) match {
      case "allow"       => Ignore
      case "error"       => Error
      case "warn" | null => Warn
      case p =>
        val message = s"Unrecognized option $p passed in for $propName. " +
          "Valid values are: {'allow', 'warn', 'error'}. Setting default to: 'warn'."
        System.err.println(message)
        Warn
    }
    private[sbt] case object Warn extends RelativeGlobViewOption
    private[sbt] case object Error extends RelativeGlobViewOption
    private[sbt] case object Ignore extends RelativeGlobViewOption
  }
  private[this] def errorMessage(relative: Glob, warn: Boolean): String = {
    val prefix = if (warn) "Warning" else "Error"
    val action =
      if (warn) "To disable this warning, "
      else "To allow implicit conversions using the current jvm working directory, "
    s"$prefix: Tried to extract the base path for relative glob $relative. $action" +
      s"re-run the program with java option, -D${RelativeGlobViewOption.propName}=allow"
  }

  /**
   * Provides extension methods for [[Glob]].
   * @param glob the [[Glob]] to augment.
   */
  implicit class GlobOps(val glob: Glob) extends AnyVal {
    private[sbt] def descendentMatches(path: Path): Boolean = glob match {
      case Pattern(p, relative) if path.startsWith(p) => relative.matches(p.getFileName)
      case g                                          => g.matches(path)
    }
    private[sbt] def toFileFilter: FileFilter = new FileFilter {
      override def accept(pathname: File): Boolean = glob.matches(pathname.toPath)
    }
    private[sbt] def toAbsolutePath(path: Path)(implicit option: RelativeGlobViewOption): Path = {
      import RelativeGlobViewOption._
      if (!path.isAbsolute) {
        option match {
          case Error  => throw new IllegalArgumentException(errorMessage(glob, warn = false))
          case Ignore => path.toAbsolutePath
          case Warn =>
            System.err.println(errorMessage(glob, warn = true))
            path.toAbsolutePath
        }
      } else path
    }
    private[sbt] def fileTreeViewListParameters(
        implicit option: RelativeGlobViewOption
    ): (Path, Int, Glob) = {
      val b = base
      val r = range._2
      val g = glob match {
        case r: RelativeGlob =>
          r.tail match {
            case Nil => Root(b)
            case t   => Pattern(b, RelativeGlob(r.tail))
          }
        case _ => glob
      }
      (b, r, g)
    }
    private[sbt] def base(implicit option: RelativeGlobViewOption): Path =
      toAbsolutePath(glob match {
        case Pattern(root, r)         => r.prefix.map(root.resolve).getOrElse(root)
        case Root(root)               => root
        case r: RelativeGlob          => r.prefix.getOrElse(Paths.get(""))
        case FullFileGlob(base, _, _) => base
      })
    private[sbt] def range: (Int, Int) = glob match {
      case Pattern(_, relative: RelativeGlob) => RelativeGlob.range(relative)
      case Root(_)                            => (0, 0)
      case relative: RelativeGlob             => RelativeGlob.range(relative)
      case FullFileGlob(_, recursive, _)      => if (recursive) (1, Int.MaxValue) else (1, 1)
    }

    /**
     * Appends a relative glob path to an existing [[Glob]]. For example:
     * {{{
     *   val glob = Glob("foo/bar")
     *   val scalaSources = glob / "**<code>/</code>*.scala"
     *   scalaSources.matches(Paths.get("foo/bar/Foo.scala")) // true
     *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Bar.scala")) // true
     *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Baz.java")) // false
     * }}}
     *
     * @param relativeGlob the string that will be parsed into a [[RelativeGlob]] and appended to
     *                     the base glob
     * @return a Glob with the new [[RelativeGlob]] appended to the query.
     */
    def /(relativeGlob: String): Glob = /(RelativeGlob.parse(relativeGlob))

    /**
     * Appends a relative glob path to an existing [[Glob]]. For example:
     * {{{
     *   val glob = Glob("foo/bar")
     *   val scalaSources = glob / "**<code>/</code>*.scala"
     *   scalaSources.matches(Paths.get("foo/bar/Foo.scala")) // true
     *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Bar.scala")) // true
     *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Baz.java")) // false
     * }}}
     *
     * @param relativeGlob the [[RelativeGlob]] to be appended to the query
     * @return a Glob with the new [[RelativeGlob]] appended to the query.
     */
    def /(relativeGlob: RelativeGlob): Glob = glob match {
      case Pattern(root, relative) => Pattern(root, relative / relativeGlob)
      case Root(path) =>
        val newRoot = relativeGlob.prefix.map(path.resolve).getOrElse(path)
        relativeGlob.tail match {
          case Nil => Root(newRoot)
          case t   => Pattern(newRoot, RelativeGlob(t))
        }
      case f: FullFileGlob =>
        throw new IllegalArgumentException(s"Can't call / on legacy glob $f")
      case r: RelativeGlob => r / relativeGlob
    }
  }

  /**
   * Converts a string to a [[Glob]].
   * @param glob the string to convert
   * @return the converted [[Glob]].
   */
  implicit def stringToGlob(glob: String): Glob = Glob(glob)

  /**
   * Provides extension methods to `java.nio.file.Path`.
   * @param path the `Path` instance to extend.
   */
  final class PathOps(val path: Path) extends AnyVal {

    /**
     * Converts the path to a single path glob
     * @return the single path [[Glob]].
     */
    def toGlob: Glob = Root(path)

    /**
     * Appends a path component to a path.
     * @param component the component to append
     * @return the appended path.
     */
    def /(component: String): Path = path.resolve(component)
  }

  /**
   * Provides extension methods to `java.nio.file.Path`
   * @param file the `File` instance to extend.
   */
  final class FileOps(val file: File) extends AnyVal {

    /**
     * Converts the file to a single path glob.
     * @return the single path [[Glob]].
     */
    def toGlob: Glob = Root(file.toPath)
  }
  private[this] val windowsEscapable = "(){}"
  private[this] val allMeta = "*{([?"
  private[file] val hasMeta: String => Boolean = _.exists(allMeta.contains(_))
  private[file] val isWin = Properties.isWin
  private[this] val splitter: String => List[String] = {
    if (Glob.isWin) { glob =>
      {
        val stringBuilder = new StringBuilder(glob.length)
        val components = new util.ArrayList[String]
        val array = glob.toCharArray
        val separator = File.separatorChar
        @tailrec def fillComponents(index: Int): Unit = index match {
          case i if i < array.length =>
            array(i) match {
              case `separator` =>
                val nextIndex = i + 1
                if (nextIndex < array.length) {
                  array(nextIndex) match {
                    case c if windowsEscapable.contains(c) =>
                      stringBuilder.append(separator).append(c)
                    case c =>
                      components.add(stringBuilder.toString)
                      stringBuilder.clear
                      stringBuilder.append(c)
                  }
                  fillComponents(nextIndex + 1)
                } else {
                  components.add(stringBuilder.toString)
                  ()
                }
              case '/' =>
                components.add(stringBuilder.toString)
                stringBuilder.clear
                fillComponents(i + 1)
              case c =>
                stringBuilder.append(c)
                fillComponents(i + 1)
            }
          case _ =>
            components.add(stringBuilder.toString)
            ()
        }
        fillComponents(index = 0)
        components.asScala.toList
      }
    } else {
      _.split(File.separatorChar).toList
    }
  }
}

/**
 * A specialization of [[Glob]] that applies only to relative paths. [[RelativeGlob]] instances
 * can always be combined with the `/` operator whereas general `Glob` instances cannot because they
 * may correspond to an absolute file path:
 * {{{
 *   val combined = Glob("/foo/bar") / RelativeGlob("
 * }}}
 */
sealed trait RelativeGlob extends Glob {
  private[file] def matchers: List[RelativeGlob.Matcher]
  private[file] def prefix: Option[Path] = matchers.takeWhile(_.isInstanceOf[PathComponent]) match {
    case Nil => None
    case (h: PathComponent) :: (t: List[PathComponent] @unchecked) =>
      Some(Paths.get(h.glob, t.map(_.glob): _*))
    case _ => None
  }
  private[file] def tail: List[RelativeGlob.Matcher] =
    matchers.dropWhile(_.isInstanceOf[PathComponent])

  /**
   * Appends additional components to this [[RelativeGlob]]:
   * {{{
   *   val glob = Glob("foo/bar")
   *   val scalaSources = glob / "**<code>/</code>*.scala"
   *   scalaSources.matches(Paths.get("foo/bar/Foo.scala")) // true
   *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Bar.scala")) // true
   *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Baz.java")) // false
   * }}}
   *
   * @param relativeGlob the additional query string to parse and append
   * @return a Glob with the new [[RelativeGlob]] appended to the query.
   */
  def /(relativeGlob: String): RelativeGlob = /(RelativeGlob.parse(relativeGlob))

  /**
   * Appends an additional [[RelativeGlob]] to this [[RelativeGlob]]:
   * {{{
   *   val glob = Glob("foo/bar")
   *   val scalaSources = glob / ** / "*.scala"
   *   scalaSources.matches(Paths.get("foo/bar/Foo.scala")) // true
   *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Bar.scala")) // true
   *   scalaSources.matches(Paths.get("foo/bar/baz/fizz/buzz/Baz.java")) // false
   * }}}
   *
   * @param relativeGlob the additional query string to parse and append
   * @return a Glob with the new [[RelativeGlob]] appended to the query.
   */
  def /(relativeGlob: RelativeGlob): RelativeGlob =
    RelativeGlob(this.matchers ::: relativeGlob.matchers)
}

/**
 * A [[RelativeGlob]] that matches any path. Can be combined with other [[Glob]] instances to build
 * recursive queries:
 * {{{
 *   val scalaSources = Glob("/foo/bar") / RecursiveGlob / "*.scala"
 *   scalaSources.matches(Paths.get("/foo/bar/Foo.scala")) // true
 *   scalaSources.matches(Paths.get("/foo/bar/baz/fizz/buzz/Bar.scala")) // true
 * }}}
 */
case object RecursiveGlob extends SingleComponentMatcher with RelativeGlob {
  override private[file] def glob: String = "**"
  def matches(path: Path): Boolean = true
}

/**
 * A [[RelativeGlob]] that matches any single path name.
 * {{{
 *   AnyPath.matches(Paths.get("foo")) // true
 *   AnyPath.matches(Paths.get("foo/bar")) // false
 *   Glob("foo/bar", AnyPath).matches(Paths.get("foo/bar")) // false
 *   Glob("foo/bar", AnyPath).matches(Paths.get("foo/bar/baz")) // true
 *   Glob("foo/bar", AnyPath).matches(Paths.get("foo/bar/baz/buzz")) // false
 * }}}
 *
 * It can be used to specify the minimum depth of a query:
 * {{{
 *   val childScalaSources = Glob("/foo/bar") / AnyPath / RecursiveGlob / "*.scala"
 *   childScalaSources.matches(Paths.get("/foo/bar/Foo.scala")) // false
 *   childScalaSources.matches(Paths.get("/foo/bar/baz/Bar.scala")) // true
 *   childScalaSources.matches(Paths.get("/foo/bar/baz/fizz/buzz/Bar.scala")) // true
 * }}}
 */
case object AnyPath extends SingleComponentMatcher with RelativeGlob {
  override private[file] def glob: String = "*"
  override def matches(path: Path): Boolean = path.getNameCount == 1
}
object RelativeGlob {

  /**
   * Aliases [[RecursiveGlob]].
   */
  val ** = RecursiveGlob

  /**
   * Aliases [[AnyPath]].
   */
  val * = AnyPath
  private[file] def parse(glob: String): RelativeGlob = Glob(glob) match {
    case r: RelativeGlob => r
    case _ =>
      val msg = s"Couldn't create relativeGlob glob from absolute glob: $glob"
      throw new IllegalArgumentException(msg)
  }
  private[sbt] def apply(matchers: String*): RelativeGlob =
    new RelativeGlobImpl(matchers.view.filterNot(_ == ".").map(Matcher.apply).toList)
  private[sbt] def apply(matchers: List[Matcher]): RelativeGlob = new RelativeGlobImpl(matchers)
  implicit val ordering: Ordering[RelativeGlob] = Ordering.by(_.matchers)
  private[file] def range(relative: RelativeGlob): (Int, Int) = {
    val res = relative.matchers.foldLeft((0, 0)) {
      case ((0, 0), RecursiveGlob)                => (1, Int.MaxValue)
      case (r @ (_, Int.MaxValue), RecursiveGlob) => r
      case ((min, Int.MaxValue), _)               => (min + 1, Int.MaxValue)
      case ((min, max), _)                        => (min + 1, max + 1)
    }
    res
  }

  private final class RelativeGlobImpl(val matchers: List[Matcher]) extends RelativeGlob {
    override def matches(path: Path): Boolean = {
      val count = path.getNameCount
      @tailrec def impl(currentIndex: Int, matchers: List[Matcher]): Boolean = matchers match {
        case RecursiveGlob :: Nil => count > 0
        case RecursiveGlob :: matchersTail =>
          currentIndex match {
            case i if i < count => recursiveMatches(matchersTail, i)
            case _              => false
          }
        case m :: Nil if currentIndex == count - 1 => m.matches(path.getFileName)
        case m :: matchersTail =>
          currentIndex match {
            case i if i < count && m.matches(path.getName(i)) => impl(i + 1, matchersTail)
            case _                                            => false
          }
        case Nil => currentIndex == count
      }
      def recursiveMatches(remaining: List[Matcher], currentIndex: Int): Boolean = {
        remaining match {
          case Nil                => true
          case nameMatcher :: Nil => nameMatcher.matches(path.getFileName)
          case _ =>
            @tailrec def recursiveImpl(index: Int): Boolean = index match {
              case i if i < count => impl(i, remaining) || recursiveImpl(i + 1)
              case _              => false
            }
            recursiveImpl(currentIndex)
        }
      }
      impl(currentIndex = 0, matchers)
    }
    override def hashCode: Int = matchers.hashCode
    override def toString: String = matchers.mkString(File.separator)
    override def equals(o: Any): Boolean = o match {
      case that: RelativeGlob => this.matchers == that.matchers
      case _                  => false
    }
  }

  private[sbt] sealed trait Matcher extends RelativeGlob
  private[sbt] object Matcher {
    private[sbt] implicit object ordering extends Ordering[Matcher] {
      override def compare(x: Matcher, y: Matcher): Int = x match {
        case RecursiveGlob =>
          y match {
            case RecursiveGlob => 0
            case _             => -1
          }
        case AnyPath =>
          y match {
            case AnyPath       => 0
            case RecursiveGlob => 1
            case _             => -1
          }
        case spm: SingleComponentMatcher =>
          y match {
            case that: SingleComponentMatcher => spm.glob.compareTo(that.glob)
            case _                            => 1
          }
        case _: FunctionNameFilter =>
          y match {
            case _: FunctionNameFilter => 0
            case _                     => 1
          }
        case nm: NotMatcher =>
          y match {
            case that: NotMatcher => compare(nm.matcher, that.matcher)
            case _                => 1
          }
        case am: AndMatcher =>
          y match {
            case that: AndMatcher =>
              compare(am.left, that.left) match {
                case 0 => compare(am.right, that.right)
                case _ => 1
              }
            case _ => 1
          }
        case om: OrMatcher =>
          y match {
            case that: OrMatcher =>
              compare(om.left, that.left) match {
                case 0 => compare(om.right, that.right)
                case _ => 1
              }
            case _ => 1
          }
      }
    }
    private[sbt] implicit val listOrdering: Ordering[List[Matcher]] = new Ordering[List[Matcher]] {
      override def compare(left: List[Matcher], right: List[Matcher]): Int = {
        val leftIt = left.iterator
        val rightIt = right.iterator
        while (leftIt.hasNext && rightIt.hasNext) {
          val res = ordering.compare(leftIt.next, rightIt.next)
          if (res != 0) return res
        }
        Ordering.Boolean.compare(leftIt.hasNext, rightIt.hasNext)
      }
    }
    private[sbt] def and(left: Matcher, right: Matcher): Matcher = {
      if (left == NoPath || right == NoPath) NoPath
      else if (left == AnyPath) right
      else if (right == AnyPath) left
      else AndMatcher(left, right)
    }
    private[sbt] def or(left: Matcher, right: Matcher): Matcher = OrMatcher(left, right)
    private[sbt] def not(matcher: Matcher): Matcher = matcher match {
      case NoPath  => AnyPath
      case AnyPath => NoPath
      case m       => NotMatcher(m)
    }
    private[sbt] def apply(glob: String): Matcher = glob match {
      case "**"                   => RecursiveGlob
      case "*"                    => AnyPath
      case g if g.startsWith("!") => NotMatcher(Matcher(g.drop(1)))
      case g if !Glob.hasMeta(g)  => PathComponent(g)
      case g                      => new GlobMatcher(g)
    }
    private[sbt] def apply(f: String => Boolean): Matcher = FunctionNameFilter(f)
  }
  private[sbt] case object NoPath extends SingleComponentMatcher with RelativeGlob {
    override def glob: String = "<null>"
    override def matches(path: Path): Boolean = false
  }
  private[file] sealed trait SingleComponentMatcher extends Matcher {
    private[file] def glob: String
    override private[file] final val matchers: List[Matcher] = this :: Nil
    override def toString: String = glob
  }
  private[file] object PathComponent {
    def apply(component: String): PathComponent = new PathComponent(component)
    def unapply(glob: Glob): Option[String] = glob match {
      case p: PathComponent => Some(p.glob)
      case _                => None
    }
  }
  private[file] final case class NotMatcher(matcher: Matcher) extends Matcher {
    override private[sbt] def matchers: List[Matcher] = this :: Nil

    /**
     * Indicates whether a path matches the pattern specified by this [[Glob]].
     *
     * @param path the path to match
     * @return true it the path matches.
     */
    override def matches(path: Path): Boolean = !matcher.matches(path)
    override def toString: String = s"!$matcher"
  }
  private[file] final case class OrMatcher(left: Matcher, right: Matcher) extends Matcher {
    override private[sbt] def matchers: List[Matcher] = this :: Nil

    /**
     * Indicates whether a path matches the pattern specified by this [[Glob]].
     *
     * @param path the path to match
     * @return true it the path matches.
     */
    override def matches(path: Path): Boolean = left.matches(path) || right.matches(path)
    override def toString: String = s"($left || $right)"
  }
  private[file] final case class AndMatcher(left: Matcher, right: Matcher) extends Matcher {
    override private[sbt] def matchers: List[Matcher] = this :: Nil

    /**
     * Indicates whether a path matches the pattern specified by this [[Glob]].
     *
     * @param path the path to match
     * @return true it the path matches.
     */
    override def matches(path: Path): Boolean = left.matches(path) && right.matches(path)
    override def toString: String = s"($left && $right)"
  }
  private[file] final case class FunctionNameFilter(f: String => Boolean) extends Matcher {
    override private[sbt] def matchers: List[Matcher] = this :: Nil
    override def matches(path: Path): Boolean = f(path.getFileName.toString)
  }
  private[file] final class PathComponent private (override val glob: String)
      extends SingleComponentMatcher {
    override def matches(path: Path): Boolean =
      path.getNameCount == 1 && path.getFileName.toString == glob
    override def equals(o: Any): Boolean = o match {
      case that: PathComponent => this.glob == that.glob
      case _                   => false
    }
    override def hashCode: Int = glob.hashCode
  }
  private final class GlobMatcher(override val glob: String) extends SingleComponentMatcher {
    private[this] val (prefix, pattern) = glob.indexOf(":") match {
      case -1 => ("glob", glob)
      case i  => (glob.substring(0, i), glob.substring(i + 1))
    }
    private[this] val matcher = FileSystems.getDefault.getPathMatcher(s"$prefix:$pattern")
    private[this] val needsHiddenFilter = prefix == "glob" && pattern.startsWith("*")
    override def matches(path: Path): Boolean =
      matcher.matches(path) && !(needsHiddenFilter && path.getFileName.toString.startsWith("."))
    override def equals(o: Any): Boolean = o match {
      case that: GlobMatcher => this.glob == that.glob
      case _                 => false
    }
    override def hashCode(): Int = glob.hashCode
  }
}
