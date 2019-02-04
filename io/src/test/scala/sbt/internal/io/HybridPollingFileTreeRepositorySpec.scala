package sbt.internal.io

import java.nio.file.{ Files, Path }
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.FileTreeDataView.Observer
import sbt.io.FileTreeRepositorySpec.FileRepositoryOps
import sbt.io._
import sbt.io.syntax._

class HybridPollingFileTreeRepositorySpec extends FlatSpec with Matchers {
  val allPass: TypedPath => Boolean = (_: TypedPath) => true
  it should "poll specified directories " in IO.withTemporaryDirectory { baseDir =>
    val dir = Files.createDirectory(baseDir.toPath.resolve("regular")).toRealPath()
    val pollingDir = Files.createDirectory(baseDir.toPath.resolve("polling")).toRealPath()
    val latch = new CountDownLatch(1)
    val repo =
      FileTreeRepository.hybrid((_: TypedPath).toPath, pollingDir.toFile ** AllPassFilter)
    try {
      repo.register(dir ** AllPassFilter)
      repo.register(pollingDir ** AllPassFilter)
      val regularFile = dir.resolve("regular-file")
      repo.addObserver(new Observer[Path] {
        override def onCreate(newEntry: FileTreeDataView.Entry[Path]): Unit = {
          if (newEntry.typedPath.toPath == regularFile) {
            latch.countDown()
          }
        }
        override def onDelete(oldEntry: FileTreeDataView.Entry[Path]): Unit = {}

        override def onUpdate(oldEntry: FileTreeDataView.Entry[Path],
                              newEntry: FileTreeDataView.Entry[Path]): Unit = {}
      })

      def listBoth: Seq[Path] = repo.ls(dir) ++ repo.ls(pollingDir)
      listBoth shouldBe 'empty
      val pollingFile = Files.createFile(pollingDir.resolve("polling-file"))
      // If we weren't polling then it would not be guaranteed that the polling file would be
      // visible yet.
      listBoth shouldBe Seq(pollingFile)
      Files.createFile(regularFile)
      // Here we need to wait for the latch to ensure that the regular file is visible to the cache.
      assert(latch.await(1, TimeUnit.SECONDS))
      listBoth.toSet shouldBe Set(pollingFile, regularFile)
    } finally {
      repo.close()
    }
  }
  it should "not return duplicates" in IO.withTemporaryDirectory { baseDir =>
    val dir = baseDir.toPath
    val subdir = Files.createDirectory(dir.resolve("subdir"))
    val nested = Files.createDirectory(subdir.resolve("nested"))
    val file = Files.createFile(nested.resolve("file"))
    val repo = FileTreeRepository.hybrid((_: TypedPath).toPath, subdir ** AllPassFilter)
    try {
      repo.register(dir ** AllPassFilter)
      repo.ls(dir).sorted shouldBe Seq(subdir, nested, file)
    } finally {
      repo.close()
    }
  }
}
