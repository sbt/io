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

import sbt.nio.file.PathFilter

@deprecated("Alternative is likely to be removed in future versions of sbt", "1.3.0")
private[sbt] trait Alternative[A, B] {
  def |(g: A => Option[B]): A => Option[B]
}

sealed trait BaseSyntax {
  implicit def singleFileFinder(file: File): PathFinder = PathFinder(file)
}
sealed abstract class IOSyntax1 extends BaseSyntax

sealed abstract class IOSyntax0 extends IOSyntax1 {
  @deprecated("Alternative is no longer used in sbt io.", "1.3.0")
  implicit def alternative[A, B](f: A => Option[B]): Alternative[A, B] = g => a => f(a) orElse g(a)
}

private[sbt] trait IOSyntax extends BaseSyntax

object syntax extends IOSyntax0 {
  type File = java.io.File
  type URI = java.net.URI
  type URL = java.net.URL

  def uri(s: String): URI = new URI(s)
  def file(s: String): File = new File(s)
  def url(s: String): URL = new URL(s)

  implicit def fileToRichFile(file: File): RichFile = new RichFile(file)
  implicit def filesToFinder(cc: Traversable[File]): PathFinder = PathFinder.strict(cc)
  implicit def fileFilterToPathFilter(file: FileFilter): PathFilter =
    PathFilter.fromFileFilter(file)
}
