package sbt.nio.file.proposal

import java.io.File
import java.nio.file.Path

import sbt.nio.file.proposal.Glob.{ FileOps, PathOps }

package object syntax extends syntax0
trait syntax0 {
  implicit def pathToPathOps(path: Path): PathOps = new PathOps(path)
  implicit def fileToFileOps(file: File): FileOps = new FileOps(file)
}
