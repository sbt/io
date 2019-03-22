package sbt.internal.io

import java.nio.file.{ Path, Paths }
import java.util
import java.util.Collections
import java.util.concurrent.{ ConcurrentHashMap, ConcurrentSkipListMap }

import sbt.internal.io.FileEvent.{ Creation, Deletion, Update }
import sbt.internal.io.FileTreeView.AllPass
import sbt.io.{ AllPassFilter, Glob }

import scala.collection.JavaConverters._
import scala.collection.mutable

private[io] class FileCache[+T](converter: (Path, SimpleFileAttributes) => CustomFileAttributes[T],
                                globs: mutable.Set[Glob]) {
  def this(converter: (Path, SimpleFileAttributes) => CustomFileAttributes[T]) =
    this(converter, ConcurrentHashMap.newKeySet[Glob].asScala)
  import FileCache._
  private[this] val files =
    Collections.synchronizedSortedMap(new ConcurrentSkipListMap[Path, CustomFileAttributes[T]])
  private[this] val view: NioFileTreeView[CustomFileAttributes[T]] =
    FileTreeView.DEFAULT.map(converter)
  private[io] def update(
      path: Path,
      attributes: SimpleFileAttributes,
  ): Seq[FileEvent[CustomFileAttributes[T]]] = {
    if (globInclude(path)) {
      files.synchronized {
        val subMap = files.subMap(path, ceiling(path))
        subMap.get(path) match {
          case null if attributes.exists =>
            add(Glob(path, (0, maxDepthForPath(path)), (_: Path) => true), attributes)
            subMap.asScala.map { case (p, a) => Creation(p, a) }.toIndexedSeq
          case null => Nil // we weren't monitoring this no longer extant path
          case p if attributes.exists =>
            Update(path, p, converter(path, attributes)) :: Nil
          case _ =>
            remove(subMap).map { case (p, a) => Deletion(p, a) }
        }
      }
    } else {
      Nil
    }
  }
  private[io] def refresh(glob: Glob): Seq[FileEvent[CustomFileAttributes[T]]] = {
    val path = glob.base
    if (globInclude(path)) {
      val subMap = files.subMap(path, ceiling(path))
      val previous = subMap.asScala.toMap
      subMap.clear()
      SimpleFileAttributes.get(path).foreach {
        case attributes if attributes.exists =>
          add(glob, attributes)
        case _ =>
      }
      val current = subMap.asScala.toMap
      val result = new util.ArrayList[FileEvent[CustomFileAttributes[T]]].asScala
      previous.foreach {
        case (p, prevAttributes) =>
          current.get(p) match {
            case Some(newAttributes) if prevAttributes != newAttributes =>
              result += Update(p, prevAttributes, newAttributes)
            case None =>
              val value = prevAttributes.value
              result += Deletion(
                p,
                CustomFileAttributes.get(
                  path,
                  SimpleFileAttributes.get(false,
                                           prevAttributes.isDirectory,
                                           prevAttributes.isRegularFile,
                                           prevAttributes.isSymbolicLink),
                  value.fold((t: Throwable) => throw t, identity)
                )
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
  private[io] def list(
      glob: Glob,
      filter: CustomFileAttributes[T] => Boolean): Seq[(Path, CustomFileAttributes[T])] = {
    files
      .subMap(glob.base, ceiling(glob.base))
      .asScala
      .filter {
        case (p, a) =>
          glob.filter(p) && filter(a)
      }
      .toIndexedSeq
  }
  private[io] def register(glob: Glob): Unit = {
    val withoutFilter = glob.withFilter(AllPassFilter)
    if (!globs.exists(_ covers withoutFilter) && globs.add(withoutFilter)) {
      SimpleFileAttributes.get(glob.base).foreach(add(withoutFilter, _))
    }
  }
  private[io] def unregister(glob: Glob): Unit = {
    if (globs.remove(glob)) {
      files.synchronized {
        val subMap = files.subMap(glob.base, ceiling(glob.base))
        val filter = globExcludes
        val toRemove = subMap.asScala.collect { case (k, _) if filter(k) => k }
        toRemove.foreach(subMap.remove)
      }
    }
  }
  private[this] def remove(subMap: util.SortedMap[Path, CustomFileAttributes[T]])
    : Seq[(Path, CustomFileAttributes[T])] = {
    val allEntries = subMap.asScala.toIndexedSeq
    allEntries.foreach { case (p, _) => subMap.remove(p) }
    allEntries
  }
  private[this] def add(glob: Glob, simpleFileAttributes: SimpleFileAttributes): Unit = {
    val newFiles = new util.HashMap[Path, CustomFileAttributes[T]]
    val asScala = newFiles.asScala
    asScala += glob.base -> converter(glob.base, simpleFileAttributes)
    if (simpleFileAttributes.isDirectory())
      newFiles.asScala ++= view.list(glob, AllPass)
    files.putAll(newFiles)
  }
  private[this] def globInclude: Path => Boolean = {
    val allGlobs = globs.toIndexedSeq
    path =>
      allGlobs.exists(_.filter(path))
  }
  private[this] def globExcludes: Path => Boolean = {
    val allGlobs = globs.toIndexedSeq
    path =>
      !allGlobs.exists(_.filter(path))
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
private[io] object FileCache {
  private implicit class GlobOps(val glob: Glob) extends AnyVal {
    def covers(other: Glob): Boolean = {
      val (left, right) = (glob.withFilter(AllPassFilter), other.withFilter(AllPassFilter))
      right.base.startsWith(left.base) && {
        (left.base == right.base && left.range._2 >= right.range._2) || {
          val depth = left.base.relativize(right.base).getNameCount
          left.range._2 >= right.range._2 - depth
        }
      }
    }
  }
}
