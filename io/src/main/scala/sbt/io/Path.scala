/* sbt
 * Copyright 2009-2015 Typesafe, Inc, Mark Harrah, and others
 */
package sbt.io

import java.io.{ File, IOException }
import java.net.URL

import scala.collection.mutable
import java.nio.file.attribute._
import java.nio.file.{
  FileSystem,
  FileVisitOption,
  FileVisitResult,
  FileVisitor,
  Files,
  LinkOption,
  Path => NioPath
}

import scala.collection.JavaConverters._

final class RichFile(val asFile: File) extends AnyVal with RichNioPath {
  def /(component: String): File = if (component == ".") asFile else new File(asFile, component)

  /** True if and only if the wrapped file exists.*/
  def exists: Boolean = asFile.exists

  /** True if and only if the wrapped file is a directory.*/
  def isDirectory: Boolean = asFile.isDirectory

  /** The last modified time of the wrapped file.*/
  def lastModified: Long = IO.getModifiedTimeOrZero(asFile)

  /**
   * True if and only if the wrapped file `asFile` exists and the file 'other'
   * does not exist or was modified before the `asFile`.
   */
  def newerThan(other: File): Boolean = Path.newerThan(asFile, other)

  /**
   * True if and only if the wrapped file `asFile` does not exist or the file `other`
   * exists and was modified after `asFile`.
   */
  def olderThan(other: File): Boolean = Path.newerThan(other, asFile)

  /** The wrapped file converted to a <code>URL</code>.*/
  def asURL: URL = asFile.toURI.toURL

  def absolutePath: String = asFile.getAbsolutePath

  /** The last component of this path.*/
  def name: String = asFile.getName

  def baseAndExt: (String, String) = {
    val nme = name
    val dot = nme.lastIndexOf('.')
    if (dot < 0) (nme, "") else (nme.substring(0, dot), nme.substring(dot + 1))
  }

  /**
   * The extension part of the name of this path.
   * This is the part of the name after the last period, or the empty string if there is no period.
   */
  def ext: String = baseAndExt._2

  /**
   * The base of the name of this path.
   * This is the part of the name before the last period, or the full name if there is no period.
   */
  def base: String = baseAndExt._1

  def relativize(sub: File): Option[File] = IO.relativizeFile(asFile, sub)
  def relativeTo(base: File): Option[File] = IO.relativizeFile(base, asFile)

  def hash: Array[Byte] = Hash(asFile)
  def hashString: String = Hash.toHex(hash)
  def hashStringHalf: String = Hash.halve(hashString)

  override def asPath: NioPath = asFile.toPath

  private[sbt] override def linkOptions: Vector[LinkOption] = Vector.empty

  def withLinkOptions(linkOption: LinkOption*): LinkOptionPath =
    new LinkOptionPath(asPath, linkOption.toVector)
}

final class LinkOptionPath(p: NioPath, lo: Vector[LinkOption]) extends RichNioPath {
  override val asPath: NioPath = p
  private[sbt] val linkOptions: Vector[LinkOption] = lo
}

sealed trait RichNioPath extends Any {
  def asPath: NioPath

  private[sbt] def linkOptions: Vector[LinkOption]

  /**
   * Returns this file's POSIX permissions.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def permissions: Set[PosixFilePermission] =
    Files.getPosixFilePermissions(asPath, linkOptions: _*).asScala.toSet

  /**
   * Returns this file's POSIX permissions.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def permissionsAsString: String =
    PosixFilePermissions.toString(permissions.asJava)

  /**
   * Updates permission of this file.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   *
   * @param permissions
   */
  def setPermissions(permissions: Set[PosixFilePermission]): Unit = {
    Files.setPosixFilePermissions(asPath, permissions.asJava)
    ()
  }

  /**
   * Adds permission to this file.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   *
   * @param permission
   */
  def addPermission(permission: PosixFilePermission): Unit =
    setPermissions(permissions + permission)

  /**
   * Removes permission from this file.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   *
   * @param permission
   */
  def removePermission(permission: PosixFilePermission): Unit =
    setPermissions(permissions - permission)

  /**
   * Tests if this file has the given permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   *
   * @param permission
   */
  def testPermission(permission: PosixFilePermission): Boolean =
    permissions(permission)

  /**
   * Tests if this file has the owner+read permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isOwnerReadable: Boolean =
    testPermission(PosixFilePermission.OWNER_READ)

  /**
   * Tests if this file has the owner+write permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isOwnerWritable: Boolean =
    testPermission(PosixFilePermission.OWNER_WRITE)

  /**
   * Tests if this file has the owner+execute permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isOwnerExecutable: Boolean =
    testPermission(PosixFilePermission.OWNER_EXECUTE)

  /**
   * Tests if this file has the group+read permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isGroupReadable: Boolean =
    testPermission(PosixFilePermission.GROUP_READ)

  /**
   * Tests if this file has the group+write permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isGroupWritable: Boolean =
    testPermission(PosixFilePermission.GROUP_WRITE)

  /**
   * Tests if this file has the group+execute permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isGroupExecutable: Boolean =
    testPermission(PosixFilePermission.GROUP_EXECUTE)

  /**
   * Tests if this file has the others+read permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isOthersReadable: Boolean =
    testPermission(PosixFilePermission.OTHERS_READ)

  /**
   * Tests if this file has the others+write permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isOthersWritable: Boolean =
    testPermission(PosixFilePermission.OTHERS_WRITE)

  /**
   * Tests if this file has the others+execute permission.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   */
  def isOthersExecutable: Boolean =
    testPermission(PosixFilePermission.OTHERS_EXECUTE)

  def attributes: BasicFileAttributes =
    Files.readAttributes(asPath, classOf[BasicFileAttributes], linkOptions: _*)

  def posixAttributes: PosixFileAttributes =
    Files.readAttributes(asPath, classOf[PosixFileAttributes], linkOptions: _*)

  def dosAttributes: DosFileAttributes =
    Files.readAttributes(asPath, classOf[DosFileAttributes], linkOptions: _*)

  def aclFileAttributeView: AclFileAttributeView =
    Files.getFileAttributeView(asPath, classOf[AclFileAttributeView], linkOptions: _*)

  /**
   * Returns the owner of a file.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   */
  def owner: UserPrincipal =
    Files.getOwner(asPath, linkOptions: _*)

  /**
   * Returns the owner of a file.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   */
  def ownerName: String = owner.getName

  /**
   * Returns the group owner of a file.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   */
  def group: GroupPrincipal = posixAttributes.group()

  /**
   * Returns the group owner of a file.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   */
  def groupName: String = group.getName

  /**
   * Updates the file owner.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   *
   * @param owner
   */
  def setOwner(owner: String): Unit = {
    val fileSystem: FileSystem = asPath.getFileSystem
    Files.setOwner(asPath, fileSystem.getUserPrincipalLookupService.lookupPrincipalByName(owner))
    ()
  }

  /**
   * Updates the group owner of the file.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   *
   * @param group
   */
  def setGroup(group: String): Unit = {
    val fileSystem: FileSystem = asPath.getFileSystem
    Files.setOwner(asPath,
                   fileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(group))
    ()
  }
}

object Path extends Mapper {
  def apply(f: File): RichFile = new RichFile(f)
  def apply(f: String): RichFile = new RichFile(new File(f))
  def fileProperty(name: String): File = new File(System.getProperty(name))
  def userHome: File = fileProperty("user.home")

  def absolute(file: File): File = new File(file.toURI.normalize).getAbsoluteFile
  def makeString(paths: Seq[File]): String = makeString(paths, File.pathSeparator)
  def makeString(paths: Seq[File], sep: String): String = {
    val separated = paths.map(_.getAbsolutePath)
    separated.find(_ contains sep).foreach(p => sys.error(s"Path '$p' contains separator '$sep'"))
    separated.mkString(sep)
  }
  def newerThan(a: File, b: File): Boolean =
    a.exists && (!b.exists || IO.getModifiedTimeOrZero(a) > IO.getModifiedTimeOrZero(b))

  /** The separator character of the platform.*/
  val sep: Char = java.io.File.separatorChar

  def toURLs(files: Seq[File]): Array[URL] = files.map(_.toURI.toURL).toArray

  private[sbt] val defaultLinkOptions: Vector[LinkOption] = Vector.empty
}

object PathFinder {

  /** A <code>PathFinder</code> that always produces the empty set of <code>Path</code>s.*/
  val empty: PathFinder = new PathFinder { def addTo(fileSet: mutable.Set[File]) = () }

  def apply(file: File): PathFinder = new SingleFile(file)

  def apply(files: => Traversable[File]): PathFinder = new PathFinder {
    def addTo(fileSet: mutable.Set[File]) = { fileSet ++= files; () }
  }

  def strict(files: Traversable[File]): PathFinder = apply(files)

}

/**
 * A path finder constructs a set of paths.
 * The set is evaluated by a call to the <code>get</code> method.
 * The set will be different for different calls to <code>get</code> if the underlying filesystem has changed.
 */
sealed abstract class PathFinder {
  import Path._
  import syntax._

  /** The union of the paths found by this <code>PathFinder</code> with the paths found by 'paths'. */
  def +++(paths: PathFinder): PathFinder = new Paths(this, paths)

  /** Excludes all paths from <code>excludePaths</code> from the paths selected by this <code>PathFinder</code>. */
  def ---(excludePaths: PathFinder): PathFinder = new ExcludeFiles(this, excludePaths)

  /**
   * Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * descendants of paths selected by this finder.
   */
  def globRecursive(filter: FileFilter): PathFinder = new DescendantOrSelfPathFinder(this, filter)

  /** Alias of globRecursive. */
  final def **(filter: FileFilter): PathFinder = globRecursive(filter)

  def allPaths: PathFinder = **(AllPassFilter)

  /**
   * Constructs a new finder that selects all paths with a name that matches <code>filter</code>
   * and are immediate children of paths selected by this finder.
   */
  def glob(filter: FileFilter): PathFinder = new ChildPathFinder(this, filter)

  /** Alias of glob */
  final def *(filter: FileFilter): PathFinder = glob(filter)

  /**
   * Constructs a new finder that selects all paths with name <code>literal</code>
   * that are immediate children of paths selected by this finder.
   */
  def /(literal: String): PathFinder = new ChildPathFinder(this, new ExactFilter(literal))

  /**
   * Constructs a new finder that selects all paths with name <code>literal</code>
   * that are immediate children of paths selected by this finder.
   */
  final def \(literal: String): PathFinder = this / literal

  /**
   * Applies `mapper` to each path selected by this PathFinder
   * and returns the path paired with the non-empty result.
   * If the result is empty (None) and `errorIfNone` is true, an exception is thrown.
   * If `errorIfNone` is false, the path is dropped from the returned Traversable.
   */
  def pair[T](mapper: File => Option[T], errorIfNone: Boolean = true): Seq[(File, T)] = {
    val apply = if (errorIfNone) mapper | fail else mapper
    for (file <- get(); mapped <- apply(file)) yield file -> mapped
  }

  /**
   * Selects all descendant paths with a name that matches <code>include</code>
   * and do not have an intermediate path with a name that matches <code>intermediateExclude</code>.
   *
   * Typical usage is <code>descendantsExcept("*.jar", ".svn")</code>
   */
  def descendantsExcept(include: FileFilter, intermediateExclude: FileFilter): PathFinder =
    (this ** include) --- (this ** intermediateExclude ** include)

  /**
   * Evaluates this finder and converts the results to a `Seq` of distinct `File`s.
   * The files returned by this method will reflect the underlying filesystem at the time of calling.
   * If the filesystem changes, two calls to this method might be different.
   */
  final def get(): Seq[File] = {
    import scala.collection.JavaConverters._
    val pathSet: mutable.Set[File] = (new java.util.LinkedHashSet[File]).asScala
    addTo(pathSet)
    pathSet.toSeq
  }

  /**
   * Only keeps paths for which `f` returns true.
   * It is non-strict, so it is not evaluated until the returned finder is evaluated.
   */
  final def filter(f: File => Boolean): PathFinder = PathFinder(get() filter f)

  /** Non-strict flatMap: no evaluation occurs until the returned finder is evaluated. */
  final def flatMap(f: File => PathFinder): PathFinder = PathFinder(get().flatMap(p => f(p).get()))

  /** Evaluates this finder and converts the results to an `Array` of `URL`s. */
  final def getURLs(): Array[URL] = get().toArray.map(_.toURI.toURL)

  /** Evaluates this finder and converts the results to a distinct sequence of absolute path strings. */
  final def getPaths(): Seq[String] = get().map(_.absolutePath)

  private[sbt] def addTo(fileSet: mutable.Set[File]): Unit

  /**
   * Create a PathFinder from this one where each path has a unique name.
   * A single path is arbitrarily selected from the set of paths with the same name.
   */
  def distinct(): PathFinder = PathFinder { get().map(p => (p.asFile.getName, p)).toMap.values }

  /**
   * Constructs a string by evaluating this finder, converting the resulting Paths to absolute path strings,
   * and joining them with the platform path separator.
   */
  final def absString(): String = Path.makeString(get())

  /** Constructs a debugging string for this finder by evaluating it and separating paths by newlines. */
  override def toString() = get().mkString("\n   ", "\n   ", "")
}

private class SingleFile(asFile: File) extends PathFinder {
  private[sbt] def addTo(fileSet: mutable.Set[File]) = if (asFile.exists) { fileSet += asFile; () }
}

private abstract class FilterFiles extends PathFinder with FileFilter {
  def parent: PathFinder
  def filter: FileFilter

  final def accept(file: File) = filter.accept(file)

  protected def handleFile(file: File, fileSet: mutable.Set[File]): Unit =
    for (matchedFile <- IO.wrapNull(file.listFiles(this)))
      fileSet += new File(file, matchedFile.getName)
}

private class DescendantOrSelfPathFinder(val parent: PathFinder, val filter: FileFilter)
    extends FilterFiles {
  private[sbt] def addTo(fileSet: mutable.Set[File]) = {
    for (file <- parent.get()) {
      if (accept(file)) fileSet += file
      handleFileDescendant(file, fileSet)
    }
  }

  private def handleFileDescendant(file: File, fileSet: mutable.Set[File]): Unit = {
    Files.walkFileTree(
      file.toPath,
      mutable.Set(FileVisitOption.FOLLOW_LINKS).asJava,
      java.lang.Integer.MAX_VALUE,
      new FileVisitor[NioPath] {
        override def preVisitDirectory(dir: NioPath,
                                       attrs: BasicFileAttributes): FileVisitResult = {
          val file = dir.toFile
          if (filter.accept(file)) fileSet += file
          FileVisitResult.CONTINUE
        }
        override def visitFile(file: NioPath, attrs: BasicFileAttributes): FileVisitResult = {
          val ioFile = file.toFile
          if (filter.accept(ioFile)) fileSet += ioFile
          FileVisitResult.CONTINUE
        }
        override def visitFileFailed(file: NioPath, exc: IOException): FileVisitResult =
          FileVisitResult.SKIP_SUBTREE
        override def postVisitDirectory(dir: NioPath, exc: IOException): FileVisitResult =
          FileVisitResult.CONTINUE
      }
    )
    ()
  }
}

private class ChildPathFinder(val parent: PathFinder, val filter: FileFilter) extends FilterFiles {
  private[sbt] def addTo(fileSet: mutable.Set[File]) =
    for (file <- parent.get())
      handleFile(file, fileSet)
}

private class Paths(a: PathFinder, b: PathFinder) extends PathFinder {
  private[sbt] def addTo(fileSet: mutable.Set[File]) = {
    a.addTo(fileSet)
    b.addTo(fileSet)
  }
}

private class ExcludeFiles(include: PathFinder, exclude: PathFinder) extends PathFinder {
  private[sbt] def addTo(pathSet: mutable.Set[File]) = {
    val includeSet = new mutable.LinkedHashSet[File]
    include.addTo(includeSet)

    val excludeSet = new mutable.HashSet[File]
    exclude.addTo(excludeSet)

    includeSet --= excludeSet
    pathSet ++= includeSet
    ()
  }
}
