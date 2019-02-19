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
import java.nio.file.{ Files, Path }

import scala.util.Try

private[sbt] class LazyFileAttributes(path: Path,
                                      simpleFileAttributes: Option[SimpleFileAttributes])
    extends BasicFileAttributes
    with SimpleFileAttributes {
  def this(path: Path) = this(path, None)
  def this(path: Path, simpleFileAttributes: SimpleFileAttributes) =
    this(path, Some(simpleFileAttributes))
  override def exists: Boolean = simpleFileAttributes.fold(attributes.fileKey != null)(_.exists)
  override def lastModifiedTime: FileTime = attributes.lastModifiedTime()
  override def lastAccessTime: FileTime = attributes.lastAccessTime()
  override def creationTime: FileTime = attributes.creationTime()
  override def isRegularFile: Boolean =
    simpleFileAttributes.fold(attributes.isRegularFile)(_.isRegularFile())
  override def isDirectory: Boolean =
    simpleFileAttributes.fold(attributes.isDirectory)(_.isDirectory())
  override def isSymbolicLink: Boolean =
    simpleFileAttributes.fold(attributes.isSymbolicLink)(_.isDirectory())
  override def isOther: Boolean = attributes.isOther
  override def size(): Long = attributes.size()
  override def fileKey(): AnyRef = attributes.fileKey()
  private[this] lazy val attributes = Try(Files.readAttributes(path, classOf[BasicFileAttributes]))
    .getOrElse(new BasicFileAttributes {
      override def lastModifiedTime(): FileTime = FileTime.fromMillis(0)
      override def lastAccessTime(): FileTime = FileTime.fromMillis(0)
      override def creationTime(): FileTime = FileTime.fromMillis(0)
      override def isRegularFile: Boolean = false
      override def isDirectory: Boolean = false
      override def isSymbolicLink: Boolean = false
      override def isOther: Boolean = false
      override def size(): Long = -1
      override def fileKey(): AnyRef = null
    })
}
