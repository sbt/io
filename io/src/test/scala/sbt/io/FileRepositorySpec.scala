package sbt.io

import java.nio.file.attribute.FileTime
import java.nio.file.{ Files, Path => JPath, Paths => JPaths }
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.FileTreeDataView.Entry
import sbt.io.FileRepositorySpec._

import scala.concurrent.duration._
import sbt.io.FileTreeView.AllPass

private[io] trait RepositoryFactory {
  def newRepository[T](converter: TypedPath => T): FileRepository[T]
}
object FileRepositorySpec {
  implicit class FileRepositoryOps[T](val fileCache: FileRepository[T]) extends AnyVal {
    def ls(path: JPath,
           maxDepth: Int = Integer.MAX_VALUE,
           filter: Entry[T] => Boolean = AllPass): Seq[JPath] =
      fileCache.listEntries(path, maxDepth, filter).map(_.path)
  }
  implicit class CountdownLatchOps(val latch: CountDownLatch) extends AnyVal {
    def await(duration: Duration): Boolean = latch.await(duration.toNanos, TimeUnit.NANOSECONDS)
  }
  def asPath(typedPath: TypedPath): JPath = typedPath.getPath
  private val DEFAULT_TIMEOUT = 1.second
  def using[T, R](fileCache: => FileRepository[T])(f: FileRepository[T] => R): R = {
    val cache = fileCache
    try f(cache)
    finally cache.close()
  }
  def withTempDir[R](f: JPath => R): R =
    IO.withTemporaryDirectory(dir => f(dir.toPath.toRealPath()))
  def withTempDir[R](dir: JPath)(f: JPath => R): R = {
    val subdir = Files.createTempDirectory(dir, "tmp")
    try f(subdir)
    finally IO.delete(subdir.toFile)
  }
  def withTempFile[R](dir: JPath)(f: JPath => R): R = {
    val file = Files.createTempFile(dir, "", "").toRealPath()
    try {
      f(file)
    } finally {
      Files.deleteIfExists(file)
      ()
    }
  }
  def withTempFile[R](f: JPath => R): R = withTempDir(withTempFile(_)(f))
  def simpleCache(f: Entry[JPath] => Unit = _ => {})(
      implicit factory: RepositoryFactory): FileRepository[JPath] = {
    val res = factory.newRepository(asPath)
    res.addObserver(f)
    res
  }
  def simpleCache(observer: FileTreeDataView.Observer[JPath])(
      implicit factory: RepositoryFactory): FileRepository[JPath] = {
    val res = factory.newRepository(asPath)
    res.addObserver(observer)
    res
  }
  case class LastModified(at: Long)
}
class FileRepositorySpec(implicit factory: RepositoryFactory) extends FlatSpec with Matchers {
  "register" should "see existing files" in withTempFile { file =>
    using(simpleCache()) { c =>
      c.register(file.getParent, Integer.MAX_VALUE)
      c.listEntries(file.getParent, Integer.MAX_VALUE, AllPass).map(_.path) shouldBe Seq(file)
    }
  }
  it should "detect new files" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    using(simpleCache((e: Entry[JPath]) => latch.countDown())) { c =>
      c.register(dir, Integer.MAX_VALUE)
      withTempFile(dir) { f =>
        assert(latch.await(DEFAULT_TIMEOUT))
        c.ls(dir, Integer.MAX_VALUE, AllPass) shouldBe Seq(f)
      }
    }
  }
  it should "detect new subdirectories" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    using(simpleCache((_: Entry[JPath]) => latch.countDown())) { c =>
      c.register(dir, Integer.MAX_VALUE)
      withTempDir(dir) { subdir =>
        assert(latch.await(DEFAULT_TIMEOUT))
        c.ls(dir, Integer.MAX_VALUE, AllPass) shouldBe Seq(subdir)
      }
    }
  }
  it should "detect move events" in withTempDir { dir =>
    val latch = new CountDownLatch(2)
    val initial = Files.createTempFile(dir, "move", "")
    val moved = JPaths.get(s"${initial.toString}.moved")
    val onChange = (_: Entry[JPath]) => latch.countDown()
    val onUpdate = (_: Entry[JPath], _: Entry[JPath]) => {}
    using(simpleCache(FileTreeDataView.Observer[JPath](onChange, onChange, onUpdate))) { c =>
      c.register(dir, maxDepth = 0)
      c.ls(dir, maxDepth = 0) === Seq(initial)
      Files.move(initial, moved)
      assert(latch.await(DEFAULT_TIMEOUT))
      c.ls(dir, maxDepth = 0) === Seq(moved)
    }
  }
  it should "ignore children of subdirectories when recursive flag is false" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      val fileLatch = new CountDownLatch(1)
      val subdirLatch = new CountDownLatch(1)
      using(simpleCache((e: Entry[JPath]) => {
        if (e.getPath.startsWith(subdir) && e.getPath != subdir) fileLatch.countDown()
        else if (e.getPath == subdir && Files.getLastModifiedTime(e.getPath).toMillis == 2000)
          subdirLatch.countDown()
      })) { c =>
        c.register(dir, maxDepth = 0)
        withTempFile(subdir) { f =>
          assert(Files.exists(f))
          assert(fileLatch.getCount == 1) // The child creation should not have triggered a callback
          Files.setLastModifiedTime(subdir, FileTime.fromMillis(2000))
          assert(subdirLatch.await(DEFAULT_TIMEOUT))
          c.ls(dir) === Seq(subdir)
        }
      }
    }
  }
  it should "add recursive flag when previously set to false" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      withTempFile(subdir) { f =>
        using(simpleCache()) { c =>
          c.register(dir, maxDepth = 0)
          c.ls(dir).toSet shouldBe Set(subdir)
          c.register(dir, Integer.MAX_VALUE)
          c.ls(dir).toSet shouldBe Set(subdir, f)
        }
      }
    }
  }
  it should "not remove recursive flag when already set" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      withTempFile(subdir) { f =>
        using(simpleCache()) { c =>
          c.register(dir, maxDepth = Integer.MAX_VALUE)
          c.ls(dir).toSet shouldBe Set(subdir, f)
          c.register(dir, maxDepth = 0)
          c.ls(dir).toSet shouldBe Set(subdir, f)
        }
      }
    }
  }

  it should "detect many creations and deletions" in withTempDir { dir =>
    val filesToAdd = 1000
    var files = Set.empty[JPath]
    val creationLatch = new CountDownLatch(filesToAdd * 2)
    val deletionLatch = new CountDownLatch(filesToAdd * 2)
    val observer = FileTreeDataView.Observer[JPath](onCreate = _ => creationLatch.countDown(),
                                                    onDelete = _ => deletionLatch.countDown(),
                                                    onUpdate = (_, _) => {})
    using(simpleCache(observer)) { c =>
      c.register(dir, maxDepth = Integer.MAX_VALUE)

      withThread("file-creation-thread") {
        files = (0 until filesToAdd).flatMap { i =>
          val subdir = Files.createTempDirectory(dir, s"subdir-$i-")
          val file = Files.createTempFile(subdir, s"file-$i-", "")
          Seq(subdir, file)
        }.toSet
      } {
        assert(creationLatch.await(DEFAULT_TIMEOUT * 10))
        c.ls(dir).toSet shouldBe files
      }

      withThread("file-deletion-thread") {
        files.foreach(p => if (Files.isDirectory(p)) IO.delete(p.toFile))
      } {
        assert(deletionLatch.await(DEFAULT_TIMEOUT * 10))
        c.ls(dir) shouldBe 'empty
      }
    }
  }

  "updates" should "be detected" in withTempFile { file =>
    val latch = new CountDownLatch(1)
    using(FileRepository.default[LastModified]((p: TypedPath) =>
      LastModified(Files.getLastModifiedTime(p.getPath).toMillis))) { c =>
      c.addObserver(
        FileTreeDataView.Observer[LastModified](
          (_: Entry[LastModified]) => {},
          (_: Entry[LastModified]) => {},
          (oldEntry: Entry[LastModified], newEntry: Entry[LastModified]) =>
            if (oldEntry != newEntry) latch.countDown()
        )
      )
      c.register(file.getParent, maxDepth = Integer.MAX_VALUE)
      val Seq(fileEntry) = c.listEntries(file.getParent, maxDepth = Integer.MAX_VALUE, AllPass)
      val lastModified = fileEntry.value
      lastModified.right.map((_: LastModified).at) shouldBe Right(
        Files.getLastModifiedTime(file).toMillis)
      val updatedLastModified = 2000L
      Files.setLastModifiedTime(file, FileTime.fromMillis(updatedLastModified))
      assert(latch.await(DEFAULT_TIMEOUT))
      val Seq(newFileEntry) = c.listEntries(file.getParent, maxDepth = Integer.MAX_VALUE, AllPass)
      newFileEntry.value.right.map(_.at) shouldBe Right(updatedLastModified)
    }
  }

  private def withThread[R](name: String)(body: => Unit)(f: => R): Unit = {
    val thread = new Thread(s"FileRepositorySpec-$name") {
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
class DefaultFileRepositorySpec
    extends FileRepositorySpec()(new RepositoryFactory {
      override def newRepository[T](converter: TypedPath => T): FileRepository[T] =
        FileRepository.default(converter)
    })
