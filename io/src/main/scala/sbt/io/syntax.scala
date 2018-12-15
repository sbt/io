package sbt.io
import sbt.internal.io.Source

private[sbt] trait Alternative[A, B] {
  def |(g: A => Option[B]): A => Option[B]
}

sealed abstract class IOSyntax1 {
  implicit def singleFileFinder(file: java.io.File): PathFinder = PathFinder(file)
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
  val * = PathFinder.*
  val ** = PathFinder.**

  def uri(s: String): URI = new URI(s)
  def file(s: String): File = new File(s)
  def url(s: String): URL = new URL(s)

  implicit def fileToRichFile(file: File): RichFile = new RichFile(file)
  implicit def filesToFinder(cc: Traversable[File]): PathFinder = PathFinder.strict(cc)
  implicit def sourceToRichSource(source: Source): RichSource = new RichSource(source)
}
