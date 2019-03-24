package sbt.internal.nio

import java.nio.file.Path
import sbt.nio.{ FileTreeView, Glob }

private[sbt] trait NioFileTreeView[+T] extends FileTreeView[(Path, T)] {
  def list(glob: Glob, filter: ((Path, T)) => Boolean): Seq[(Path, T)]
  final def list(glob: Glob, filter: (Path, T) => Boolean): Seq[(Path, T)] =
    list(glob, filter.tupled)
}
