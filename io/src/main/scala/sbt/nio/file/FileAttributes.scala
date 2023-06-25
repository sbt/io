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

package sbt.nio.file

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ Files, LinkOption, NoSuchFileException, Path => NioPath }

/**
 * Represents a minimal set of attributes a file. In contrast to
 * `java.nio.file.attribute.BasicFileAttributes`, it is possible to compute the values provided
 * by this trait without stating the file. If all of the methods return false, the user may infer
 * that the file to which these attributes corresponds does not exist. An instance of this class
 * may not represent that current state of the file if the underlying file has been modified since
 * the instance was first created.
 */
sealed trait FileAttributes {

  /**
   * Returns true if the underlying file is a regular file.
   * @return true if the underlying file is a regular file.
   */
  def isRegularFile: Boolean

  /**
   * Returns true if the underlying file is a directory.
   * @return true if the underlying file is a directory.
   */
  def isDirectory: Boolean

  /**
   * Returns true if the underlying file is a symbolic link.
   * @return true if the underlying file is a symbolic link.
   */
  def isSymbolicLink: Boolean

  /**
   * Returns true if the underlying file is not a regular file, directory or symbolic link. The
   * type of this file is thus platform dependent. For example, on linux, it could be a named
   * pipe.
   * @return true if the underlying file is not a regular file, directory or symbolic link.
   */
  def isOther: Boolean
}

object FileAttributes {
  case object NonExistent extends FileAttributes {
    override def isRegularFile: Boolean = false
    override def isDirectory: Boolean = false
    override def isSymbolicLink: Boolean = false
    override def isOther: Boolean = false
  }
  private final class FileAttributesImpl(
      override val isDirectory: Boolean,
      override val isOther: Boolean,
      override val isRegularFile: Boolean,
      override val isSymbolicLink: Boolean
  ) extends FileAttributes {
    override def hashCode: Int =
      (((isRegularFile.hashCode * 31) ^ isDirectory.hashCode) * 31) ^ isSymbolicLink.hashCode
    override def equals(o: Any): Boolean = o match {
      case that: FileAttributesImpl =>
        this.isDirectory == that.isDirectory &&
          this.isOther == that.isOther &&
          this.isRegularFile == that.isRegularFile &&
          this.isSymbolicLink == that.isSymbolicLink
      case _ => false
    }
    override def toString: String =
      s"FileAttributes(isDirectory = $isDirectory, isOther = $isOther," +
        s"isRegularFile = $isRegularFile, isSymbolicLink = $isSymbolicLink)"
  }
  def apply(path: NioPath): Either[IOException, FileAttributes] = apply(path, followLinks = true)
  private[sbt] def apply(path: NioPath, followLinks: Boolean): Either[IOException, FileAttributes] =
    try {
      val attrs =
        Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      if (attrs.isSymbolicLink && followLinks) {
        try {
          val linkAttrs = Files.readAttributes(path, classOf[BasicFileAttributes])
          Right(
            apply(
              linkAttrs.isDirectory,
              linkAttrs.isOther,
              linkAttrs.isRegularFile,
              isSymbolicLink = true
            )
          )
        } catch {
          case _: NoSuchFileException =>
            Right(
              apply(
                isDirectory = false,
                isOther = false,
                isRegularFile = false,
                isSymbolicLink = true
              )
            )
        }
      } else
        Right(apply(attrs.isDirectory, attrs.isOther, attrs.isRegularFile, attrs.isSymbolicLink))
    } catch {
      case _: NoSuchFileException => Right(NonExistent)
      case e: IOException         => Left(e)
    }
  def apply(
      isDirectory: Boolean,
      isOther: Boolean,
      isRegularFile: Boolean,
      isSymbolicLink: Boolean
  ): FileAttributes =
    new FileAttributesImpl(isDirectory, isOther, isRegularFile, isSymbolicLink)
}
