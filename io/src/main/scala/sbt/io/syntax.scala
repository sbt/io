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

package sbt.io

import java.io.File

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
  def url(s: String): URL = uri(s).toURL

  implicit def fileToRichFile(file: File): RichFile = new RichFile(file)
  implicit def filesToFinder(cc: Traversable[File]): PathFinder = PathFinder.strict(cc)
}
