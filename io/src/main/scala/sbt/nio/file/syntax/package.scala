package sbt.nio.file

import java.io.File
import java.nio.file.Path

import sbt.nio.file.Glob.{ FileOps, PathOps }

package object syntax extends syntax0
private[sbt] trait syntax0 {
  implicit def pathToPathOps(path: Path): PathOps = new PathOps(path)
  implicit def fileToFileOps(file: File): FileOps = new FileOps(file)
}
