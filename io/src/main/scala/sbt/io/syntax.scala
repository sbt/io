package sbt.io

import java.io.File

import sbt.io.PathFinder.Combinator.SingleFilePathFinderCombinator

private[sbt] trait Alternative[A, B] {
  def |(g: A => Option[B]): A => Option[B]
}

sealed trait IOSyntax2 {
  implicit def lowPriorityFileFinder(file: File): PathFinder = PathFinder(file)
}
sealed abstract class IOSyntax1 extends IOSyntax2 {
  @deprecated(
    "1.3.0",
    "The api of singleFileFinder is now implemented by two implicit defs: " +
      "singleFileGlobBuilder and singleFilePathFinderCombinator. Prefer these two imports, but " +
      "if a PathFinder is explicitly required, then import lowPriorityFileFinder (this conversion " +
      "from File to PathFinder may not be supported in sbt 2"
  )
  def singleFileFinder(file: File): PathFinder = PathFinder(file)
  implicit def singleFileGlobBuilder(file: File): GlobBuilder[Glob] = new Glob.Builder(file)
  implicit def singleFilePathFinderCombinator(file: File): PathFinder.Combinator =
    new SingleFilePathFinderCombinator(file)
  implicit def singleFileToGlob(file: File): ToGlob = new Glob.Builder(file)
}

sealed abstract class IOSyntax0 extends IOSyntax1 {
  implicit def alternative[A, B](f: A => Option[B]): Alternative[A, B] = new Alternative[A, B] {
    def |(g: A => Option[B]) = a => f(a) orElse g(a)
  }
}

object syntax extends IOSyntax0 {
  type File = java.io.File
  type URI = java.net.URI
  type URL = java.net.URL

  def uri(s: String): URI = new URI(s)
  def file(s: String): File = new File(s)
  def url(s: String): URL = new URL(s)

  implicit def fileToRichFile(file: File): RichFile = new RichFile(file)
  implicit def filesToFinder(cc: Traversable[File]): PathFinder = PathFinder.strict(cc)
}
