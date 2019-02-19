/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.io

import java.nio.file.attribute.{ BasicFileAttributes, FileTime }
import java.nio.file.{ Files, NoSuchFileException, Path => NioPath }

import scala.util.Try
import scala.util.control.NonFatal

/**
 * Represents a subset of BasicFileAttributes that can sometimes be evaluated without running
 * stat on the file as part of listing a directory.
 */
private[sbt] trait SimpleFileAttributes {
  // Ignore warnings about dropping the parens. We can't mix SimpleFileAttributes with
  // BasicFileAttributes if these have nullary param lists.
  def isDirectory(): Boolean
  def isRegularFile(): Boolean
  def isSymbolicLink(): Boolean
  def exists: Boolean
}
private[sbt] object SimpleFileAttributes {
  def get(path: NioPath): Try[SimpleFileAttributes] =
    Try {
      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
      new Impl(true, attrs.isDirectory, attrs.isRegularFile, attrs.isSymbolicLink)
    }.recover {
      case _: NoSuchFileException => new Impl(false, false, false, false)
    }
  private[sbt] def get(exists: Boolean,
                       isDirectory: Boolean,
                       isRegularFile: Boolean,
                       isSymbolicLink: Boolean): SimpleFileAttributes =
    new Impl(exists, isDirectory, isRegularFile, isSymbolicLink)
  private class Impl(override val exists: Boolean,
                     override val isDirectory: Boolean,
                     override val isRegularFile: Boolean,
                     override val isSymbolicLink: Boolean)
      extends SimpleFileAttributes {
    override def equals(o: Any): Boolean = o match {
      case that: SimpleFileAttributes =>
        (this.isDirectory == that.isDirectory) && (this.isRegularFile == that.isRegularFile) && (this.isSymbolicLink == that.isSymbolicLink)
      case _ => false
    }
    override def hashCode: Int =
      ((isDirectory.hashCode * 31) ^ (isRegularFile.hashCode * 31)) ^ (isSymbolicLink.hashCode * 31)
    override def toString: String =
      s"SimpleFileAttributes(isDirectory = $isDirectory, isRegularFile = $isRegularFile" +
        s", isSymbolicLink = $isSymbolicLink)"
  }
}

private[sbt] trait CustomFileAttributes[+T] extends SimpleFileAttributes with BasicFileAttributes {
  def value: Either[Throwable, T]
}
private[sbt] object CustomFileAttributes {
  private[sbt] def get[T](path: NioPath,
                          simpleFileAttributes: SimpleFileAttributes,
                          t: T): CustomFileAttributes[T] = {
    val attrs = new LazyFileAttributes(path, Some(simpleFileAttributes))
    new Impl(Right(t), simpleFileAttributes.exists, attrs)
  }
  private[sbt] def get[T](path: NioPath,
                          simpleFileAttributes: SimpleFileAttributes,
                          f: (NioPath, SimpleFileAttributes) => T): CustomFileAttributes[T] = {
    val attrs = new LazyFileAttributes(path, Some(simpleFileAttributes))
    val value = try Right(f(path, attrs))
    catch { case NonFatal(t) => Left(t) }
    new Impl(value, simpleFileAttributes.exists, attrs)
  }
  private class Impl[T](override val value: Either[Throwable, T],
                        override val exists: Boolean,
                        attributes: BasicFileAttributes)
      extends CustomFileAttributes[T] {
    override def lastModifiedTime(): FileTime = attributes.lastModifiedTime()
    override def lastAccessTime(): FileTime = attributes.lastAccessTime()
    override def creationTime: FileTime = attributes.creationTime()
    override def isRegularFile(): Boolean = attributes.isRegularFile
    override def isDirectory(): Boolean = attributes.isDirectory
    override def isSymbolicLink(): Boolean = attributes.isSymbolicLink
    override def isOther: Boolean = attributes.isOther
    override def size(): Long = attributes.size()
    override def fileKey(): AnyRef = attributes.fileKey()
    override def equals(o: Any): Boolean = o match {
      case that: CustomFileAttributes[_] =>
        (this.isDirectory == that.isDirectory) && (this.isRegularFile == that.isRegularFile) &&
          (this.isSymbolicLink == that.isSymbolicLink) && (this.exists && that.exists) &&
          (this.value == that.value)
      case _ => false
    }
    override def hashCode: Int =
      ((isDirectory().hashCode * 31) ^ (isRegularFile().hashCode * 31)) ^ (isSymbolicLink().hashCode * 31)
    override def toString: String =
      s"CustomFileAttributes(isDirectory = ${isDirectory()}, isRegularFile = ${isRegularFile()}" +
        s", isSymbolicLink = ${isSymbolicLink()}, exists = $exists, value = $value)"
  }
}
