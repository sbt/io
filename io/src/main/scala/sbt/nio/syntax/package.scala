package sbt
package nio

import java.nio.file.Path

import sbt.nio.Glob.GlobOps

package object syntax extends syntax0
private[sbt] trait syntax0 {
  implicit def pathToGlobOps(path: Path): GlobOps = new GlobOps.PathOps(path)
}
