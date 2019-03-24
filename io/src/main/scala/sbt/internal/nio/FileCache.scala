package sbt.internal.nio

import java.nio.file.{ Path, Paths }
import java.util
import java.util.Collections
import java.util.concurrent.{ ConcurrentHashMap, ConcurrentSkipListMap }

import sbt.internal.nio.FileEvent.{ Creation, Deletion, Update }
import sbt.nio.FileAttributes.NonExistent
import sbt.nio.FileTreeView._
import sbt.nio.{ AllPass, FileAttributes, FileTreeView, Glob }

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Try

private[nio] class FileCache[+T](converter: (Path, FileAttributes) => Try[T],
                                 globs: mutable.Set[Glob]) {
  def this(converter: (Path, FileAttributes) => Try[T]) =
    this(converter, ConcurrentHashMap.newKeySet[Glob].asScala)
  import FileCache._
  private[this] val files =
    Collections.synchronizedSortedMap(new ConcurrentSkipListMap[Path, (FileAttributes, Try[T])])
  private[this] val view: NioFileTreeView[(FileAttributes, Try[T])] =
    FileTreeView.DEFAULT_NIO.map((p: Path, a: FileAttributes) => a -> converter(p, a))
  private[nio] def update(
      path: Path,
      attributes: FileAttributes,
  ): Seq[FileEvent[(FileAttributes, Try[T])]] = {
    if (globInclude(path)) {
      files.synchronized {
        val subMap = files.subMap(path, ceiling(path))
        val exists = attributes != NonExistent
        subMap.get(path) match {
          case null if exists =>
            add(Glob(path, (0, maxDepthForPath(path)), (_: String) => true), attributes)
            subMap.asScala.map { case (p, a) => Creation(p, a) }.toIndexedSeq
          case null => Nil // we weren't monitoring this no longer extant path
          case prev if exists =>
            Update(path, prev, attributes -> converter(path, attributes)) :: Nil
          case _ =>
            remove(subMap).map { case (p, a) => Deletion(p, a) }
        }
      }
    } else {
      Nil
    }
  }
  private[nio] def refresh(glob: Glob): Seq[FileEvent[(FileAttributes, Try[T])]] = {
    val path = glob.base
    if (globInclude(path)) {
      val subMap = files.subMap(path, ceiling(path))
      val previous = subMap.asScala.toMap
      subMap.clear()
      FileAttributes(path).foreach(add(glob, _))
      val current = subMap.asScala.toMap
      val result = new util.ArrayList[FileEvent[(FileAttributes, Try[T])]].asScala
      previous.foreach {
        case (p, prevPair) =>
          current.get(p) match {
            case Some(newPair) if prevPair != newPair =>
              result += Update(p, prevPair, newPair)
            case None =>
              val (attrs, value) = prevPair
              result += Deletion(
                p,
                FileAttributes(isDirectory = attrs.isDirectory,
                               isOther = false,
                               isRegularFile = attrs.isRegularFile,
                               isSymbolicLink = attrs.isSymbolicLink) -> value
              )
            case _ =>
          }
      }
      current.foreach {
        case (p, newAttributes) =>
          previous.get(p) match {
            case Some(prevAttributes) if prevAttributes != newAttributes =>
              result += Update(p, prevAttributes, newAttributes)
            case None => result += Creation(p, newAttributes)
            case _    =>
          }
      }
      result.toVector
    } else {
      Nil
    }
  }
  private[nio] def list(
      glob: Glob,
      filter: (FileAttributes, Try[T]) => Boolean): Seq[(Path, (FileAttributes, Try[T]))] = {
    files
      .subMap(glob.base, ceiling(glob.base))
      .asScala
      .filter {
        case (p, (a, v)) =>
          glob.filter.accept(p) && filter(a, v)
      }
      .toIndexedSeq
  }
  private[nio] def register(glob: Glob): Unit = {
    val withoutFilter = Glob(glob.base, glob.range, AllPass)
    if (!globs.exists(_ covers withoutFilter) && globs.add(withoutFilter)) {
      FileAttributes(glob.base).foreach(add(withoutFilter, _))
    }
  }
  private[nio] def unregister(glob: Glob): Unit = {
    if (globs.remove(glob)) {
      files.synchronized {
        val subMap = files.subMap(glob.base, ceiling(glob.base))
        val filter = globExcludes
        val toRemove = subMap.asScala.collect { case (k, _) if filter(k) => k }
        toRemove.foreach(subMap.remove)
      }
    }
  }
  private[this] def remove(subMap: util.SortedMap[Path, (FileAttributes, Try[T])])
    : Seq[(Path, (FileAttributes, Try[T]))] = {
    val allEntries = subMap.asScala.toIndexedSeq
    allEntries.foreach { case (p, _) => subMap.remove(p) }
    allEntries
  }
  private[this] def add(glob: Glob, fileAttributes: FileAttributes): Unit = {
    if (fileAttributes != NonExistent) {
      val newFiles = new util.HashMap[Path, (FileAttributes, Try[T])]
      val asScala = newFiles.asScala
      asScala += (glob.base -> (fileAttributes -> converter(glob.base, fileAttributes)))
      if (fileAttributes.isDirectory)
        newFiles.asScala ++= view.list(glob, _ => true)
      files.putAll(newFiles)
    }
  }
  private[this] def globInclude: Path => Boolean = {
    val allGlobs = globs.toIndexedSeq
    path =>
      allGlobs.exists(_.filter.accept(path))
  }
  private[this] def globExcludes: Path => Boolean = {
    val allGlobs = globs.toIndexedSeq
    path =>
      !allGlobs.exists(_.filter.accept(path))
  }
  private[this] def maxDepthForPath(path: Path): Int = {
    globs.toIndexedSeq.view
      .map(g =>
        if (path.startsWith(g.base)) {
          if (path == g.base) g.range._2
          else
            g.range._2 match {
              case Int.MaxValue => Int.MaxValue
              case d            => d - g.base.relativize(path).getNameCount
            }
        } else Int.MinValue)
      .min
  }
  // This is a mildly hacky way of specifying an upper bound for children of a path
  private[this] def ceiling(path: Path): Path = Paths.get(path.toString + Char.MaxValue)
}
private[nio] object FileCache {
  private implicit class GlobOps(val glob: Glob) extends AnyVal {
    def covers(other: Glob): Boolean = {
      val left = Glob(glob.base, glob.range, AllPass)
      val right = Glob(other.base, other.range, AllPass)
      right.base.startsWith(left.base) && {
        (left.base == right.base && left.range._2 >= right.range._2) || {
          val depth = left.base.relativize(right.base).getNameCount
          left.range._2 >= right.range._2 - depth
        }
      }
    }
  }
}
