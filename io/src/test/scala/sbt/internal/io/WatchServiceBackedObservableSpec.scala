package sbt
package internal
package io

import java.nio.file.{ Files, Path, WatchKey }
import java.util.concurrent.{ ConcurrentHashMap, CountDownLatch, TimeUnit }

import org.scalatest.FlatSpec
import sbt.io._
import sbt.io.syntax._

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
      new WatchServiceBackedObservable(watchState,
                                       100.millis,
                                       (_: Path, _: SimpleFileAttributes) => (),
                                       closeService = true,
                                       new WatchLogger {
                                         override def debug(msg: => Any): Unit = {}
                                       })
    try {
      val latch = new CountDownLatch(1)
      val file = subdir.resolve("file")
      observable.addObserver(new Observer[(Path, Unit)] {
        override def onNext(t: (Path, Unit)): Unit = if (t._1 == file) latch.countDown()
      })
      observable.register(path ** AllPassFilter)
      Files.createFile(file)
      assert(latch.await(1, TimeUnit.SECONDS))
    } finally observable.close()
  }
}
