package sbt.internal.io

import java.nio.file.attribute.FileTime
import java.nio.file.{ Files, Path => NioPath, Paths => NioPaths }
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.internal.io.FileTreeView.AllPass
import sbt.io.syntax._
import sbt.io.{ AllPassFilter, Glob, IO }

import scala.concurrent.duration._

object FileTreeRepositorySpec {
  implicit class FileRepositoryOps[T](val fileCache: FileTreeRepository[CustomFileAttributes[T]])
      extends AnyVal {
    def ls(glob: Glob,
           filter: (NioPath, CustomFileAttributes[T]) => Boolean = (_, _) => true): Seq[NioPath] =
      fileCache.list(glob, filter).map(_._1)
  }
  implicit class CountdownLatchOps(val latch: CountDownLatch) extends AnyVal {
    def await(duration: Duration): Boolean = latch.await(duration.toNanos, TimeUnit.NANOSECONDS)
  }
  private val DEFAULT_TIMEOUT = 1.second
  def using[T, R](fileCache: => FileTreeRepository[T])(f: FileTreeRepository[T] => R): R = {
    val cache = fileCache
    try f(cache)
    finally cache.close()
  }
  def withTempDir[R](f: NioPath => R): R =
    IO.withTemporaryDirectory(dir => f(dir.toPath.toRealPath()))
  def withTempDir[R](dir: NioPath)(f: NioPath => R): R = {
    val subdir = Files.createTempDirectory(dir, "tmp")
    try f(subdir)
    finally IO.delete(subdir.toFile)
  }
  def withTempFile[R](dir: NioPath)(f: NioPath => R): R = {
    val file = Files.createTempFile(dir, "", "").toRealPath()
    try {
      f(file)
    } finally {
      Files.deleteIfExists(file)
      ()
    }
  }
  def withTempFile[R](f: NioPath => R): R = withTempDir(withTempFile(_)(f))
  def simpleCache(f: NioPath => Unit): FileTreeRepository[CustomFileAttributes[Unit]] =
    simpleCache(new Observer[(NioPath, FileEvent[CustomFileAttributes[Unit]])] {
      override def onNext(t: (NioPath, FileEvent[CustomFileAttributes[Unit]])): Unit = f(t._1)
    })
  def simpleCache(observer: Observer[(NioPath, FileEvent[CustomFileAttributes[Unit]])])
    : FileTreeRepository[CustomFileAttributes[Unit]] = {
    val underlying = new FileTreeRepositoryImpl((path: NioPath, attributes: SimpleFileAttributes) =>
      CustomFileAttributes.get(path, attributes, ()))
    underlying.addObserver(observer)
    underlying
  }
  case class LastModified(at: Long)
}
class FileTreeRepositorySpec extends FlatSpec with Matchers {
  import FileTreeRepositorySpec._
  "register" should "see existing files" in withTempFile { file =>
    using(simpleCache((_: NioPath) => {})) { c =>
      val glob = file.getParent ** AllPassFilter
      c.register(glob)
      c.ls(glob) shouldBe Seq(file)
    }
  }
  it should "detect new files" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    val file = dir.resolve("file")
    using(simpleCache((p: NioPath) => if (p == file) latch.countDown())) { c =>
      c.register(dir ** AllPassFilter)
      Files.createFile(file)
      assert(latch.await(DEFAULT_TIMEOUT))
      c.ls(dir ** AllPassFilter) shouldBe Seq(file)
    }
  }
  it should "detect new subdirectories" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    val subdir = dir.resolve("subdir")
    using(simpleCache((p: NioPath) => if (p == subdir) latch.countDown())) { c =>
      c.register(dir ** AllPassFilter)
      Files.createDirectories(subdir)
      assert(latch.await(DEFAULT_TIMEOUT))
      c.ls(dir ** AllPassFilter) shouldBe Seq(subdir)
    }
  }
  it should "detect move events" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    val initial = Files.createTempFile(dir, "move", "")
    val moved = NioPaths.get(s"${initial.toString}.moved")
    val onChange = (p: NioPath, _: CustomFileAttributes[Unit]) => if (p == moved) latch.countDown()
    val onUpdate =
      (_: (NioPath, CustomFileAttributes[Unit]), pair: (NioPath, CustomFileAttributes[Unit])) =>
        if (pair._1 == moved) latch.countDown()
    using(simpleCache(FileEvent.observer(onChange, onChange, onUpdate))) { c =>
      c.register(dir ** AllPassFilter)
      c.ls(dir * AllPassFilter) === Seq(initial)
      Files.move(initial, moved)
      assert(latch.await(DEFAULT_TIMEOUT))
      c.ls(dir * AllPassFilter) === Seq(moved)
    }
  }
  it should "ignore children of subdirectories when recursive flag is false" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      val fileLatch = new CountDownLatch(1)
      val subdirLatch = new CountDownLatch(1)
      using(simpleCache((path: NioPath) => {
        if (path.startsWith(subdir) && path != subdir) fileLatch.countDown()
        else if (path == subdir && Files.getLastModifiedTime(path).toMillis == 2000)
          subdirLatch.countDown()
      })) { c =>
        c.register(dir * AllPassFilter)
        withTempFile(subdir) { f =>
          assert(Files.exists(f))
          assert(fileLatch.getCount == 1) // The child creation should not have triggered a callback
          Files.setLastModifiedTime(subdir, FileTime.fromMillis(2000))
          assert(subdirLatch.await(DEFAULT_TIMEOUT))
          c.ls(dir ** AllPassFilter) === Seq(subdir)
        }
      }
    }
  }
  it should "add recursive flag when previously set to false" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      withTempFile(subdir) { f =>
        using(simpleCache((_: NioPath) => {})) { c =>
          c.register(dir * AllPassFilter)
          c.ls(dir ** AllPassFilter).toSet shouldBe Set(subdir)
          c.register(dir ** AllPassFilter)
          c.ls(dir ** AllPassFilter).toSet shouldBe Set(subdir, f)
        }
      }
    }
  }
  it should "not remove recursive flag when already set" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      withTempFile(subdir) { f =>
        using(simpleCache((_: NioPath) => {})) { c =>
          c.register(dir ** AllPassFilter)
          c.ls(dir ** AllPassFilter).toSet shouldBe Set(subdir, f)
          c.register(dir * AllPassFilter)
          c.ls(dir ** AllPassFilter).toSet shouldBe Set(subdir, f)
        }
      }
    }
  }

  it should "detect many creations and deletions" in withTempDir { dir =>
    val filesToAdd = 1000
    var files = Set.empty[NioPath]
    val creationLatch = new CountDownLatch(filesToAdd * 2)
    val deletionLatch = new CountDownLatch(filesToAdd * 2)
    val observer =
      FileEvent.observer[CustomFileAttributes[Unit]](onCreate = (_, _) => creationLatch.countDown(),
                                                     onDelete = (_, _) => deletionLatch.countDown(),
                                                     onUpdate = (_, _) => {})
    using(simpleCache(observer)) { c =>
      c.register(dir ** AllPassFilter)

      withThread("file-creation-thread") {
        files = (0 until filesToAdd).flatMap { i =>
          val subdir = Files.createTempDirectory(dir, s"subdir-$i-")
          val file = Files.createTempFile(subdir, s"file-$i-", "")
          Seq(subdir, file)
        }.toSet
      } {
        assert(creationLatch.await(DEFAULT_TIMEOUT * 10))
        c.ls(dir ** AllPassFilter).toSet shouldBe files
      }

      withThread("file-deletion-thread") {
        files.foreach(p => if (Files.isDirectory(p)) IO.delete(p.toFile))
      } {
        assert(deletionLatch.await(DEFAULT_TIMEOUT * 10))
        c.ls(dir * AllPassFilter) shouldBe 'empty
      }
    }
  }

  "updates" should "be detected" in withTempFile { file =>
    val latch = new CountDownLatch(1)
    val updatedLastModified = 2000L
    using(FileTreeRepository.default[LastModified] {
      case (p: NioPath, a: SimpleFileAttributes) =>
        CustomFileAttributes.get(p, a, LastModified(Files.getLastModifiedTime(p).toMillis))
    }) { c =>
      c.addObserver(new Observer[(NioPath, FileEvent[CustomFileAttributes[LastModified]])] {
        override def onNext(t: (NioPath, FileEvent[CustomFileAttributes[LastModified]])): Unit = {
          val (_, event) = t
          val attributes = event.attributes
          if (attributes.exists && attributes.value.map(_.at) == Right(updatedLastModified))
            latch.countDown()
        }
      })
      c.register(file.getParent ** AllPassFilter)
      val Seq(fileEntry) = c.list(file.getParent ** AllPassFilter, AllPass)
      val lastModified = fileEntry._2.value
      lastModified.map((_: LastModified).at) shouldBe Right(
        Files.getLastModifiedTime(file).toMillis)
      Files.setLastModifiedTime(file, FileTime.fromMillis(updatedLastModified))
      assert(latch.await(DEFAULT_TIMEOUT))
      val Seq(newFileEntry) = c.list(file.getParent ** AllPassFilter, AllPass)
      newFileEntry._2.value.map(_.at) shouldBe Right(updatedLastModified)
    }
  }
  private def withThread[R](name: String)(body: => Unit)(f: => R): Unit = {
    val thread = new Thread(s"FileTreeRepositorySpec-$name") {
      override def run(): Unit = body
      setDaemon(true)
      start()
    }
    try {
      f
      ()
    } finally {
      thread.interrupt()
      thread.join(5000L)
    }
  }
}
