package sbt.internal.io

import java.io.IOException
import java.nio.file.{ ClosedWatchServiceException, Files, Path, Paths }

import org.scalatest.{ Assertion, FlatSpec, Matchers }
import sbt.internal.io.EventMonitorSpec._
import sbt.io.FileEventMonitor.Event
import sbt.io.FileTreeDataView.{ Entry, Observable, Observer }
import sbt.io.syntax._
import sbt.io.{ FileTreeDataView, NullWatchLogger, TypedPath, WatchService, _ }

import scala.annotation.tailrec
import scala.concurrent.duration._

private[sbt] trait EventMonitorSpec { self: FlatSpec with Matchers =>
  def pollDelay: FiniteDuration
  def newObservable(source: Seq[Source]): Observable[_]
  def newObservable(file: File): Observable[_] =
    newObservable(Seq(Source(file.toPath.toRealPath().toFile)))
  private val maxWait = 2 * pollDelay

  it should "detect modified files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "Foo.scala"

    writeNewFile(file, "foo")

    watchTest(parentDir) {
      IO.write(file, "bar")
    }
  }

  it should "watch a directory for file creation" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "NewSource.scala"

    IO.createDirectory(parentDir)

    watchTest(parentDir) {
      IO.write(created, "foo")
    }
  }

  it should "ignore creation of directories with no tracked sources" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val created = parentDir / "ignoreme"

      IO.createDirectory(parentDir)

      watchTest(parentDir, expectedTrigger = false) {
        IO.createDirectory(created)
      }
  }

  it should "ignore creation of files that do not match inclusion filter" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val created = parentDir / "ignoreme"

      IO.createDirectory(parentDir)

      watchTest(parentDir, expectedTrigger = false) {
        IO.touch(created)
      }
    }

  it should "ignore creation of files that are explicitly ignored" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val created = parentDir / ".hidden.scala"

      IO.createDirectory(parentDir)

      watchTest(parentDir, expectedTrigger = false) {
        IO.touch(created)
      }
  }

  it should "ignore creation of an empty directory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "ignoreme"

    IO.createDirectory(parentDir)

    watchTest(parentDir, expectedTrigger = false) {
      IO.createDirectory(created)
    }
  }

  it should "detect files created in a subdirectory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / "NewSource.scala"

    IO.createDirectory(subDir)

    watchTest(parentDir) {
      IO.write(created, "foo")
    }
  }

  it should "ignore creation of files not included in inclusion filter in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "sub"
      val created = subDir / "ignoreme"

      IO.createDirectory(subDir)

      watchTest(parentDir, expectedTrigger = false) {
        IO.touch(created)
      }
    }

  it should "ignore creation of files explicitly ignored in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "sub"
      val created = subDir / ".hidden.scala"

      IO.createDirectory(subDir)

      watchTest(parentDir, expectedTrigger = false) {
        IO.touch(created)
      }
    }

  it should "ignore creation of empty directories in a subdirectory" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "sub"
      val created = subDir / "ignoreme"

      IO.createDirectory(subDir)

      watchTest(parentDir, expectedTrigger = false) {
        IO.createDirectory(created)
      }
  }

  it should "detect deleted files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "WillBeDeleted.scala"
    IO.write(file, "foo")

    watchTest(parentDir) {
      IO.delete(file)
    }
  }

  it should "ignore deletion of files not included in inclusion filter" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val file = parentDir / "ignoreme"
      IO.write(file, "foo")

      watchTest(parentDir, expectedTrigger = false) {
        IO.delete(file)
      }
    }

  it should "ignore deletion of files explicitly ignored" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / ".hidden.scala"
    IO.write(file, "foo")

    watchTest(parentDir, expectedTrigger = false) {
      IO.delete(file)
    }
  }

  it should "ignore deletion of empty directories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "ignoreme"
    IO.createDirectory(subDir)

    watchTest(parentDir, expectedTrigger = false) {
      IO.delete(subDir)
    }
  }

  it should "detect deleted files in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / "WillBeDeleted.scala"
    IO.write(willBeDeleted, "foo")

    watchTest(parentDir) {
      IO.delete(willBeDeleted)
    }
  }

  it should "ignore deletion of files not included in inclusion filter in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val willBeDeleted = subDir / "ignoreme"
      IO.write(willBeDeleted, "foo")

      watchTest(parentDir, expectedTrigger = false) {
        IO.delete(willBeDeleted)
      }
    }

  it should "ignore deletion of files explicitly ignored in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val willBeDeleted = subDir / ".hidden.scala"
      IO.write(willBeDeleted, "foo")

      watchTest(parentDir, expectedTrigger = false) {
        IO.delete(willBeDeleted)
      }
    }

  it should "ignore deletion of empty directories in subdirectories" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val willBeDeleted = subDir / "ignoreme"
      IO.createDirectory(willBeDeleted)

      watchTest(parentDir, expectedTrigger = false) {
        IO.delete(willBeDeleted)
      }
  }

  it should "ignore creation and then deletion of empty directories" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      IO.createDirectory(parentDir)

      val observable = newObservable(parentDir.scalaSource)
      val monitor = FileEventMonitor(observable)
      try {
        val triggered0 = watchTest(monitor) {
          IO.createDirectory(subDir)
        }
        triggered0 shouldBe false

        val triggered1 = watchTest(monitor) {
          IO.delete(subDir)
        }
        triggered1 shouldBe false
      } finally {
        monitor.close()
      }
  }

  it should "detect deletion of a directory containing watched files" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val src = subDir / "src.scala"

      IO.createDirectory(parentDir)
      val observable = newObservable(parentDir.scalaSource)
      val logger = new CachingWatchLogger
      val monitor = FileEventMonitor(observable, logger)
      try {
        val triggered0 = watchTest(monitor) {
          IO.createDirectory(subDir)
          IO.touch(src)
        }
        triggered0 shouldBe true

        val triggered1 = watchTest(monitor) {
          IO.delete(subDir)
        }
        if (!triggered1) logger.printLines("Did not trigger when expected")
        triggered1 shouldBe true
      } finally monitor.close()
  }

  it should "not generate multiple events for the same file within anti-entropy period" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val file = parentDir / "Foo.scala"

      writeNewFile(file, "foo")
      val observable = newObservable(parentDir)
      // Choose a very long anti-entropy period to ensure that the second trigger doesn't happen
      val logger = new CachingWatchLogger
      val monitor =
        FileEventMonitor.antiEntropy(observable, 10.seconds, logger, 50.millis, 10.minutes)
      try {
        val triggered0 = watchTest(monitor) {
          IO.write(file, "bar")
        }
        assert(triggered0)
        assert(IO.read(file) == "bar")

        val deadline = maxWait.fromNow
        @tailrec
        def poll(): Boolean = {
          val wait = deadline - Deadline.now
          monitor.poll(wait) match {
            case sources if sources.map(_.entry.typedPath.toPath).contains(file.toPath) => true
            case _ if Deadline.now < deadline                                           => poll()
            case _                                                                      => false
          }
        }
        monitor.drain(maxWait)
        IO.write(file, "baz")
        val triggered1 = poll()
        if (triggered1) logger.printLines("Unexpected trigger during anti-entropy period.")
        assert(!triggered1)
        assert(IO.read(file) == "baz")
      } finally {
        monitor.close()
      }
    }

  it should "ignore valid files in non-recursive subdirectories" in IO.withTemporaryDirectory {
    dir =>
      val file = dir / "src" / "Foo.scala"
      val source =
        Source(dir.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith(".")))
          .withRecursive(false)
      val observable = newObservable(Seq(source))
      val monitor = FileEventMonitor(observable)
      try {
        val triggered = watchTest(monitor) {
          IO.write(file, "foo")
        }
        assert(!triggered)
        assert(IO.read(file) == "foo")
      } finally {
        monitor.close()
      }
  }

  it should "log triggered files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "Foo.scala"

    writeNewFile(file, "foo")

    val sources = Seq(
      Source(parentDir.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith("."))))
    var lines: Seq[String] = Nil
    val logger = new WatchLogger {
      override def debug(msg: => Any): Unit = lines.synchronized {
        lines :+= msg.toString
      }
    }
    val observable = newObservable(sources)
    val monitor = FileEventMonitor(observable, logger)
    try {
      val triggered = watchTest(monitor) {
        IO.write(file, "bar")
      }
      assert(triggered)
      assert(lines.exists(_.startsWith("Received")))
    } finally {
      monitor.close()
    }
  }

  it should "handle rapid creation of many subdirectories and files" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      Files.createDirectories(parentDir.toPath)
      val realParent = parentDir.toPath.toRealPath()
      val subdirCount = 200
      val subdirFileCount = 4

      // Longer timeout because there are many file system operations. This can be very expensive
      // especially in the PollingWatchSpec since both the PollingWatchService and the
      // WatchServiceBackedObserable overflow handler are hammering the file system. To minimize the
      // conflicts, we set a long interval between polls in the PollingWatchService using
      // getServiceWithPollDelay. The timeout was increased from 20.seconds to 40.seconds to address
      // transient failures of this test on Appveyor windows builds.
      val observable = newObservable(realParent.toFile)
      val logger = new CachingWatchLogger
      val monitor = FileEventMonitor(observable, logger)
      try {
        val subdirs =
          (1 to subdirCount).map(i => Files.createDirectories(realParent.resolve(s"subdir-$i")))
        val files = subdirs.flatMap { subdir =>
          subdir +: (1 to subdirFileCount).map { j =>
            Files.write(subdir.resolve(s"file-$j.scala"), s"foo".getBytes)
          }
        }
        val allPaths = files.toSet
        val lastFile = files.last

        // There is a bug in older java versions where sometimes the watch context can be incorrect
        // when we are simultaneously watching and registering new paths. It is fixed in the latest
        // openjdk 8 and openjdk and oracle >= 9, but in CI we may expect th
        // DefaultWatchServiceSpec to occasionally fail.
        // http://blog.omega-prime.co.uk/2015/11/14/beware-java-nio-file-watchservice-is-subtly-broken-on-linux/
        val triggeredPaths =
          monitor.drain(maxWait * 4).map(_.entry.typedPath.toPath).toSet.intersect(allPaths)
        if (triggeredPaths != allPaths) {
          logger.printLines("Triggered paths did not contain all of the expected paths")
          val diff = allPaths diff triggeredPaths
          if (diff.size > 5)
            println(diff.take(5).mkString("", "\n", s"\n and ${diff.size - 5} more ..."))
          else println(diff mkString "\n")
          // tolerate the failures on linux until we upgrade travis to java 9 or greater
          assert(System.getProperty("os.name", "").startsWith("Linux"))
        }

        assert(IO.read(lastFile.toFile) == s"foo")
        IO.write(lastFile.toFile, "baz")
        val updates = monitor.drain(maxWait * 4)
        val result = updates.exists(_.entry.typedPath.toPath == lastFile)
        assert(result)
        assert(IO.read(lastFile.toFile) == "baz")
      } finally {
        monitor.close()
      }
  }

  def watchTest(monitor: FileEventMonitor[_])(modifier: => Unit): Boolean = {
    modifier
    monitor.poll(maxWait * 2).nonEmpty
  }

  def watchTest(base: File, expectedTrigger: Boolean = true)(modifier: => Unit): Assertion = {
    val sources = Seq(
      Source(base.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith("."))))
    val observable = newObservable(sources)
    try {
      val logger = new CachingWatchLogger
      val triggered = watchTest(FileEventMonitor(observable, logger))(modifier)
      if (triggered != expectedTrigger) logger.printLines(s"Expected $expectedTrigger")
      triggered shouldBe expectedTrigger
    } finally {
      observable.close()
    }
  }

  @tailrec
  final def writeNewFile(file: File, content: String, attempt: Int = 0): Unit = {
    if (attempt == 0) IO.write(file, content)
    // IO.setModifiedTimeOrFalse sometimes throws an invalid argument exception
    val res = try {
      IO.setModifiedTimeOrFalse(file, (Deadline.now - 5.seconds).timeLeft.toMillis)
    } catch { case _: IOException if attempt < 10 => false }
    if (!res) writeNewFile(file, content, attempt + 1)
  }

}

object EventMonitorSpec {
  // This can't be defined in MonitorOps because of a bug in the scala 2.10 compiler
  @tailrec
  private def drain(monitor: FileEventMonitor[_],
                    duration: FiniteDuration,
                    events: Seq[Event[_]]): Seq[Event[_]] = {
    val newEvents = monitor.poll(duration)
    if (newEvents.isEmpty) events else drain(monitor, duration, events ++ newEvents)
  }
  implicit class MonitorOps(val monitor: FileEventMonitor[_]) extends AnyVal {
    def drain(duration: FiniteDuration, events: Seq[Event[_]] = Nil): Seq[Event[_]] =
      EventMonitorSpec.drain(monitor, duration, events)
  }
  implicit class FileOps(val file: File) extends AnyVal {
    def scalaSource: Seq[Source] =
      Seq(Source(file.toPath.toRealPath().toFile, "*.scala", HiddenFileFilter))
  }
  private class FilteredObservable[T](observable: Observable[T], filter: Entry[T] => Boolean)
      extends Observable[T] {
    override def addObserver(observer: FileTreeDataView.Observer[T]): Int =
      observable.addObserver(new Observer[T] {
        override def onCreate(newEntry: Entry[T]): Unit =
          if (filter(newEntry)) observer.onCreate(newEntry)
        override def onDelete(oldEntry: Entry[T]): Unit =
          if (filter(oldEntry)) observer.onDelete(oldEntry)
        override def onUpdate(oldEntry: Entry[T], newEntry: Entry[T]): Unit = {
          if (filter(newEntry)) observer.onUpdate(oldEntry, newEntry)
        }
      })
    override def removeObserver(handle: Int): Unit = observable.removeObserver(handle)
    override def close(): Unit = observable.close()
  }
  implicit class ObservableOps[T](val observable: Observable[T]) extends AnyVal {
    def filter(f: Entry[T] => Boolean): Observable[T] = new FilteredObservable[T](observable, f)
  }
  class CachingWatchLogger extends WatchLogger {
    val lines = new scala.collection.mutable.ArrayBuffer[String]
    override def debug(msg: => Any): Unit = lines.synchronized { lines += msg.toString; () }
    def printLines(msg: String) = println(s"$msg. Log lines:\n${lines mkString "\n"}")
  }
}

class FileTreeRepositoryEventMonitorSpec extends FlatSpec with Matchers with EventMonitorSpec {
  override def pollDelay: FiniteDuration = 100.millis

  override def newObservable(sources: Seq[Source]): Observable[_] = {
    val repository = FileTreeRepository.default((_: TypedPath).toPath)
    sources foreach (s =>
      repository.register(s.base.toPath, if (s.recursive) Integer.MAX_VALUE else 0))
    repository.filter(e => sources.exists(s => s.accept(e.typedPath.toPath)))
  }
}
abstract class SourceModificationWatchSpec(
    getServiceWithPollDelay: FiniteDuration => WatchService,
    override val pollDelay: FiniteDuration
) extends FlatSpec
    with Matchers
    with EventMonitorSpec {
  def getService = getServiceWithPollDelay(10.milliseconds)

  "WatchService.poll" should "throw a `ClosedWatchServiceException` if used after `close`" in {
    val service = getService
    service.close()
    assertThrows[ClosedWatchServiceException](service.poll(1.second))
  }

  "WatchService.register" should "throw a `ClosedWatchServiceException` if used after `close`" in {
    val service = getService
    service.close()
    assertThrows[ClosedWatchServiceException](service.register(Paths.get(".")))
  }

  "WatchService.close" should "not throw if called multiple times" in {
    val service = getService
    service.close()
    service.close()
  }

  override def newObservable(sources: Seq[Source]): FileTreeDataView.Observable[_] = {
    val watchState = WatchState.empty(getService, sources)
    val observable = new WatchServiceBackedObservable[Path](watchState,
                                                            5.millis,
                                                            (_: TypedPath).toPath,
                                                            closeService = true,
                                                            NullWatchLogger)
    observable.filter((entry: Entry[Path]) => {
      val path = entry.typedPath.toPath
      sources.exists(_.accept(path))
    })
  }
}
