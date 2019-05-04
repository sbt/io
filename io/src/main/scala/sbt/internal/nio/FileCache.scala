package sbt.internal.nio

import java.nio.file.{ Path, Paths }
import java.util
import java.util.Collections
import java.util.concurrent.{ ConcurrentHashMap, ConcurrentSkipListMap }

import sbt.internal.nio.FileEvent.{ Creation, Deletion, Update }
import sbt.nio.file.FileAttributes.NonExistent
import sbt.nio.file.{ AnyPath, FileAttributes, FileTreeView, Glob, RecursiveGlob }

import scala.collection.JavaConverters._
import scala.collection.mutable

private[nio] class FileCache[+T](converter: Path => T, globs: mutable.Set[Glob]) {
  def this(converter: Path => T) =
    this(converter, ConcurrentHashMap.newKeySet[Glob].asScala)
  import FileCache._
  private[this] val files =
    Collections.synchronizedSortedMap(new ConcurrentSkipListMap[Path, T])
  private[this] val view: FileTreeView.Nio[FileAttributes] = FileTreeView.default
  private[nio] def update(
      path: Path,
      attributes: FileAttributes,
  ): Seq[FileEvent[T]] = {
    if (globInclude(path)) {
      files.synchronized {
        val subMap = files.subMap(path, ceiling(path))
        val exists = attributes != NonExistent
        subMap.get(path) match {
          case null if exists =>
            add(updateGlob(path), attributes)
            subMap.asScala.map { case (p, a) => Creation(p, a) }.toIndexedSeq
          case null => Nil // we weren't monitoring this no longer extant path
          case prev if exists =>
            Update(path, prev, converter(path)) :: Nil
          case _ =>
            remove(subMap).map { case (p, a) => Deletion(p, a) }
        }
      }
    } else {
      Nil
    }
  }
  private[nio] def refresh(glob: Glob): Seq[FileEvent[T]] = synchronized {
    val path = glob.base
    if (globInclude(path)) {
      val subMap = files.subMap(path, ceiling(path))
      val previous = subMap.asScala.toMap
      subMap.clear()
      FileAttributes(path).foreach(add(glob, _))
      val current = subMap.asScala.toMap
      val result = new util.ArrayList[FileEvent[T]].asScala
      previous.foreach {
        case (p, prev) =>
          current.get(p) match {
            case Some(newPair) if prev != newPair =>
              result += Update(p, prev, newPair)
            case None => result += Deletion(p, prev)
            case _    =>
          }
      }
      current.foreach {
        case (p, newAttributes) =>
          previous.get(p) match {
            case None => result += Creation(p, newAttributes)
            case _    =>
          }
      }
      result.toVector
    } else {
      Nil
    }
  }
  private[nio] def list(glob: Glob): Seq[(Path, T)] = {
    files
      .subMap(glob.base, ceiling(glob.base))
      .asScala
      .toIndexedSeq
  }
  private[nio] def register(glob: Glob): Unit = {
    val unfiltered = glob.range._2 match {
      case Int.MaxValue => Glob(glob.base, RecursiveGlob)
      case d            => (1 to d).foldLeft(Glob(glob.base)) { case (g, _) => g / AnyPath }
    }
    if (!globs.exists(_ covers unfiltered) && globs.add(unfiltered)) {
      FileAttributes(glob.base).foreach(add(unfiltered, _))
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
  private[this] def remove(subMap: util.SortedMap[Path, T]): Seq[(Path, T)] = {
    val allEntries = subMap.asScala.toIndexedSeq
    allEntries.foreach { case (p, _) => subMap.remove(p) }
    allEntries
  }
  private[this] def add(glob: Glob, fileAttributes: FileAttributes): Unit = {
    if (fileAttributes != NonExistent) {
      val newFiles = new util.HashMap[Path, T]
      val asScala = newFiles.asScala
      asScala += (glob.base -> converter(glob.base))
      if (fileAttributes.isDirectory)
        newFiles.asScala ++= view.list(glob).map { case (p, _) => p -> converter(p) }
      files.putAll(newFiles)
    }
  }
  private[this] def globInclude: Path => Boolean = { path =>
    globs.exists(g => g.matches(path) || g.base == path)
  }
  private[this] def globExcludes: Path => Boolean = { path =>
    !globs.exists(g => g.matches(path) || g.base == path)
  }
  private[this] def updateGlob(path: Path): Glob = {
    val depth = globs.toIndexedSeq.view
      .map(
        g =>
          if (path.startsWith(g.base)) {
            if (path == g.base) g.range._2
            else
              g.range._2 match {
                case Int.MaxValue => Int.MaxValue
                case d            => d - g.base.relativize(path).getNameCount
              }
          } else Int.MinValue
      )
      .min
    depth match {
      case Int.MaxValue => Glob(path, RecursiveGlob)
      case d            => (1 to d).foldLeft(Glob(path)) { case (g, _) => g / AnyPath }
    }
  }
  // This is a mildly hacky way of specifying an upper bound for children of a path
  private[this] def ceiling(path: Path): Path = Paths.get(path.toString + Char.MaxValue)
}
private[nio] object FileCache {
  private implicit class GlobOps(val glob: Glob) extends AnyVal {
    def covers(other: Glob): Boolean = {
      val (leftBase, rightBase) = (glob.base, other.base)
      val (leftMaxDepth, rightMaxDepth) = (glob.range._2, other.range._2)
      rightBase.startsWith(leftBase) && {
        (leftBase == rightBase && leftMaxDepth >= rightMaxDepth) || {
          val depth = leftBase.relativize(rightBase).getNameCount
          leftMaxDepth >= rightMaxDepth - depth
        }
      }
    }
  }
}
