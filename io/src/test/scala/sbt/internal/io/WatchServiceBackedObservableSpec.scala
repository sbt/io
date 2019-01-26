package sbt
package internal
package io

import java.nio.file.{ Files, Path, WatchKey }
import java.util.concurrent.{ ConcurrentHashMap, CountDownLatch, TimeUnit }

import org.scalatest.FlatSpec
import sbt.io.FileTreeDataView.Observer
import sbt.io._

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class WatchServiceBackedObservableSpec extends FlatSpec {
  "register" should "work recursively" in IO.withTemporaryDirectory { dir =>
    val path = dir.toPath
    val subdir = Files.createDirectories(path.resolve("a").resolve("b").resolve("c")).toRealPath()
    val watchState =
      new NewWatchState(ConcurrentHashMap.newKeySet[Glob].asScala,
                        WatchService.default,
                        new ConcurrentHashMap[Path, WatchKey].asScala)
    val observable =
      new WatchServiceBackedObservable[Path](watchState,
                                             100.millis,
                                             (_: TypedPath).toPath,
                                             closeService = true,
                                             new WatchLogger {
                                               override def debug(msg: => Any): Unit = {}
                                             })
    try {
      val latch = new CountDownLatch(1)
      val file = subdir.resolve("file")
      observable.addObserver(new Observer[Path] {
        override def onCreate(newEntry: FileTreeDataView.Entry[Path]): Unit =
          if (newEntry.typedPath.toPath == file) latch.countDown()
        override def onDelete(oldEntry: FileTreeDataView.Entry[Path]): Unit = {}
        override def onUpdate(oldEntry: FileTreeDataView.Entry[Path],
                              newEntry: FileTreeDataView.Entry[Path]): Unit = {}
      })
      observable.register(path, Int.MaxValue)
      Files.createFile(file)
      assert(latch.await(1, TimeUnit.SECONDS))
    } finally observable.close()
  }
}
