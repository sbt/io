package sbt.internal.io

import java.io.IOException
import java.nio.file.{ ClosedWatchServiceException, Files, Path, Paths }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.internal.io.EventMonitorSpec._
import sbt.internal.nio.FileEvent.Deletion
import sbt.internal.nio.{
  FileEvent,
  FileEventMonitor,
  FileTreeRepository,
  Observable,
  Observer,
  Observers,
  Registerable,
  WatchLogger,
  WatchServiceBackedObservable
}
import sbt.io.syntax._
import sbt.io.{ WatchService, _ }
import sbt.nio.{ FileAttributes, Glob }

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{ Success, Try }

private[sbt] trait EventMonitorSpec { self: FlatSpec with Matchers =>
  def pollDelay: FiniteDuration
  def newObservable(glob: Seq[Glob], logger: Logger): Observable[Event]
  def newObservable(file: File): Observable[Event] =
    newObservable(Seq(file.toPath.toRealPath().toFile ** AllPassFilter), NullLogger)
  private val maxWait = 2 * pollDelay
  private[this] val random = new scala.util.Random()
  private def randomTouch(file: File, add: Boolean = true): Unit = {
    IO.touch(file)
    val rand = (10000 + random.nextInt(50000)) * (if (add) 1 else -1)
    IO.setModifiedTimeOrFalse(file, System.currentTimeMillis + rand)
    ()
  }

  it should "detect modified files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "Foo.scala"

    writeNewFile(file, "foo")
    val real = realPath(file.toPath)

    assert(watchTest(parentDir, pathFilter(real), contains(real)) {
      IO.write(file, "bar")
    })
  }

  it should "watch a directory for file creation" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "NewSource.scala"

    IO.createDirectory(parentDir)

    assert(watchTest(parentDir, pathFilter(created.toPath), contains(created.toPath)) {
      IO.write(created, "foo")
    })
  }

  it should "ignore creation of directories with no tracked globs" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val created = parentDir / "ignoreme"
      val subdir = parentDir / "subdir"
      val subFile = subdir / "foo.scala"

      IO.createDirectory(subdir)

      assert(watchTest(parentDir, AllPass, excludes(created.toPath)) {
        IO.createDirectory(created)
        Files.createFile(subFile.toPath)
        ()
      })
    }

  it should "ignore creation of files that do not match inclusion filter" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val created = parentDir / "ignoreme"
      val scalaCreated = parentDir / "foo.scala"

      IO.createDirectory(parentDir)

      assert(watchTest(parentDir, AllPass, excludes(created.toPath)) {
        randomTouch(created)
        randomTouch(scalaCreated)
      })
    }

  it should "ignore creation of files that are explicitly ignored" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val created = parentDir / ".hidden.scala"
      val notIgnored = parentDir / "foo.scala"

      IO.createDirectory(parentDir)

      assert(watchTest(parentDir, AllPass, excludes(created.toPath)) {
        randomTouch(created)
        randomTouch(notIgnored)
      })
    }

  it should "ignore creation of an empty directory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "ignoreme"
    val source = parentDir / "foo.scala"

    IO.createDirectory(parentDir)

    assert(watchTest(parentDir, AllPass, excludes(created.toPath)) {
      IO.createDirectory(created)
      randomTouch(source)
    })
  }

  it should "detect files created in a subdirectory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / "NewSource.scala"

    IO.createDirectory(subDir)

    assert(watchTest(parentDir, pathFilter(created.toPath), contains(created.toPath)) {
      IO.write(created, "foo")
    })
  }

  it should "ignore creation of files not included in inclusion filter in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "sub"
      val created = subDir / "ignoreme"
      val source = subDir / "foo.scala"

      IO.createDirectory(subDir)

      assert(watchTest(parentDir, AllPass, excludes(created.toPath)) {
        randomTouch(created)
        randomTouch(source)
      })
    }

  it should "ignore creation of files explicitly ignored in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "sub"
      val created = subDir / ".hidden.scala"
      val source = subDir / "foo.scala"

      IO.createDirectory(subDir)

      assert(watchTest(parentDir, AllPass, excludes(created.toPath)) {
        randomTouch(created)
        randomTouch(source)
      })
    }

  it should "ignore creation of empty directories in a subdirectory" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "sub"
      val created = subDir / "ignoreme"
      val source = subDir / "foo.scala"

      IO.createDirectory(subDir)

      assert(watchTest(parentDir, AllPass, excludes(created.toPath)) {
        IO.createDirectory(created)
        randomTouch(source)
      })
    }

  it should "detect deleted files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "WillBeDeleted.scala"
    IO.write(file, "foo")

    assert(watchTest(parentDir, isDeletion(file.toPath), hasDeletion(file.toPath)) {
      IO.delete(file)
    })
  }

  it should "ignore deletion of files not included in inclusion filter" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val file = parentDir / "ignoreme"
      IO.write(file, "foo")
      val source = parentDir / "foo.scala"

      assert(watchTest(parentDir, AllPass, excludes(file.toPath)) {
        IO.delete(file)
        randomTouch(source)
      })
    }

  it should "ignore deletion of files explicitly ignored" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / ".hidden.scala"
    IO.write(file, "foo")
    val source = parentDir / "foo.scala"

    assert(watchTest(parentDir, AllPass, excludes(file.toPath)) {
      IO.delete(file)
      randomTouch(source)
    })
  }

  it should "ignore deletion of empty directories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "ignoreme"
    IO.createDirectory(subDir)
    val source = parentDir / "foo.scala"

    assert(watchTest(parentDir, AllPass, excludes(subDir.toPath)) {
      IO.delete(subDir)
      randomTouch(source)
    })
  }

  it should "detect deleted files in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / "WillBeDeleted.scala"
    IO.write(willBeDeleted, "foo")

    assert(
      watchTest(parentDir, isDeletion(willBeDeleted.toPath), hasDeletion(willBeDeleted.toPath)) {
        IO.delete(willBeDeleted)
      })
  }

  it should "ignore deletion of files not included in inclusion filter in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val willBeDeleted = subDir / "ignoreme"
      IO.write(willBeDeleted, "foo")
      val source = subDir / "foo.scala"

      assert(watchTest(parentDir, AllPass, excludes(willBeDeleted.toPath)) {
        IO.delete(willBeDeleted)
        randomTouch(source)
      })
    }

  it should "ignore deletion of files explicitly ignored in subdirectories" in
    IO.withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val willBeDeleted = subDir / ".hidden.scala"
      IO.write(willBeDeleted, "foo")
      val source = subDir / "foo.scala"

      assert(watchTest(parentDir, AllPass, excludes(willBeDeleted.toPath)) {
        IO.delete(willBeDeleted)
        randomTouch(source)
      })
    }

  it should "ignore deletion of empty directories in subdirectories" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val willBeDeleted = subDir / "ignoreme"
      IO.createDirectory(willBeDeleted)
      val source = parentDir / "foo.scala"

      assert(watchTest(parentDir, AllPass, excludes(willBeDeleted.toPath)) {
        IO.delete(willBeDeleted)
        randomTouch(source)
      })
    }

  it should "ignore creation and then deletion of empty directories" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      IO.createDirectory(parentDir)
      val source = parentDir / "foo.scala"

      val observable = newObservable(parentDir.scalaSourceGlobs, NullLogger)
      val monitor = FileEventMonitor(observable)
      IO.touch(source)
      try {
        assert(watchTest(monitor, AllPass, excludes(subDir.toPath)) {
          IO.createDirectory(subDir)
          randomTouch(source)
          ()
        })

        assert(watchTest(monitor, AllPass, excludes(subDir.toPath)) {
          IO.delete(subDir)
          randomTouch(source, add = false)
          ()
        })
      } finally {
        monitor.close()
      }
    }

  it should "detect deletion of a directory containing watched files" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val src = subDir / "src.scala"

      IO.createDirectory(parentDir)
      val observable = newObservable(parentDir.scalaSourceGlobs, NullLogger)
      val logger = new CachingWatchLogger
      val monitor = FileEventMonitor(observable, logger)
      try {
        assert(watchTest(monitor, AllPass, excludes(subDir.toPath)) {
          IO.createDirectory(subDir)
          randomTouch(src)
        })

        val triggered = watchTest(monitor, AllPass, AllPass) {
          IO.delete(subDir)
        }
        if (!triggered) logger.printLines("Did not trigger when expected")
        assert(triggered)
      } finally monitor.close()
    }

  it should "not generate multiple events for the same file within anti-entropy period" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val file = parentDir / "Foo.scala"
      val otherFile = parentDir / "bar.scala"

      writeNewFile(file, "foo")
      val observable = newObservable(parentDir)
      // Choose a very long anti-entropy period to ensure that the second trigger doesn't happen
      val logger = new CachingWatchLogger
      val monitor =
        FileEventMonitor.antiEntropy(observable, 10.seconds, logger, 50.millis, 10.minutes)
      try {
        assert(watchTest(monitor, pathFilter(file.toPath), includesOnly(file.toPath)) {
          IO.write(file, "bar")
        })
        assert(IO.read(file) == "bar")

        val triggered1 = watchTest(monitor, pathFilter(file.toPath), contains(file.toPath)) {
          IO.write(file, "baz")
          IO.touch(otherFile)
        }
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
      val globs = dir.toPath.toRealPath().toFile * "*.scala" :: Nil
      val observable = newObservable(globs, NullLogger)
      val monitor = FileEventMonitor(observable)
      val valid = dir / "foo.scala"
      try {
        assert(watchTest(monitor, AllPass, includesOnly(valid.toPath)) {
          IO.write(file, "foo")
          randomTouch(valid)
        })
        assert(IO.read(file) == "foo")
      } finally {
        monitor.close()
      }
  }

  it should "log triggered files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "Foo.scala"

    writeNewFile(file, "foo")

    val globs = parentDir.toPath.toRealPath().toFile.scalaSourceGlobs
    var lines: Seq[String] = Nil
    val logger: WatchLogger = msg => lines.synchronized(lines :+= msg.toString)
    val observable = newObservable(globs, NullLogger)
    val monitor = FileEventMonitor(observable, logger)
    try {
      assert(watchTest(monitor, AllPass, includesOnly(file.toPath)) {
        IO.write(file, "bar")
      })
      assert(lines.exists(_.startsWith("Received")))
    } finally {
      monitor.close()
    }
  }

  it should "handle rapid creation of many subdirectories and files" in IO
    .withTemporaryDirectory { dir =>
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
          monitor.drain(maxWait * 4).map(_.path).toSet.intersect(allPaths)
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
        val result = updates.exists(_.path == lastFile)
        assert(result)
        assert(IO.read(lastFile.toFile) == "baz")
      } finally {
        monitor.close()
      }
    }

  def watchTest(monitor: FileEventMonitor[FileEvent[_]],
                filter: FileEvent[_] => Boolean,
                check: Seq[FileEvent[_]] => Boolean)(modifier: => Unit): Boolean = {
    modifier
    val events = monitor.poll(maxWait * 2, filter)
    check(events)
  }

  def watchTest(base: File, filter: FileEvent[_] => Boolean, check: Seq[FileEvent[_]] => Boolean)(
      modifier: => Unit): Boolean = {
    val globs = base.toPath.toRealPath().toFile.scalaSourceGlobs
    val logger = new CachingWatchLogger
    val observable: Observable[Event] = newObservable(globs, logger)
    try {
      watchTest(FileEventMonitor(observable, logger), filter, check)(modifier)
    } finally {
      observable.close()
    }
  }

  @tailrec
  final def writeNewFile(file: File, content: String, attempt: Int = 0): Unit = {
    if (attempt == 0) IO.write(file, content)
    // IO.setModifiedTimeOrFalse sometimes throws an invalid argument exception
    val res = try {
      IO.setModifiedTimeOrFalse(file, (Deadline.now - 5.seconds).time.toMillis)
    } catch { case _: IOException if attempt < 10 => false }
    if (!res) writeNewFile(file, content, attempt + 1)
  }

}

object EventMonitorSpec {
  type Event = FileEvent[FileAttributes]
  val AllPass: Any => Boolean = ((_: Any) => true).label("AllPass")
  val NonEmpty: Seq[FileEvent[_]] => Boolean = ((_: Seq[FileEvent[_]]).nonEmpty).label("NonEmpty")
  private implicit class LabeledFunction[T, R](val f: T => R) extends AnyVal {
    def label(string: String): T => R = new (T => R) {
      override def apply(t: T): R = f(t)
      override def toString: String = string
    }
  }
  @tailrec
  final def realPath(path: Path, fileName: Option[Path] = None): Path = {
    val res: Path = try path.toRealPath()
    catch { case _: IOException => null }
    if (res != null) fileName.fold(res)(res.resolve)
    else {
      val newFileName = path.getFileName
      realPath(path.getParent, fileName.map(newFileName.resolve) orElse Some(newFileName))
    }
  }
  def pathFilter(path: Path): FileEvent[_] => Boolean = {
    val real = realPath(path)
    ((_: FileEvent[_]).path == real).label(s"PathFilter($real)")
  }
  def contains(path: Path): Seq[FileEvent[_]] => Boolean = {
    val real = realPath(path)
    ((_: Seq[FileEvent[_]]).exists(_.path == real)).label(s"Contains($real)")
  }
  def excludes(path: Path): Seq[FileEvent[_]] => Boolean = {
    val real = realPath(path)
    ((s: Seq[FileEvent[_]]) => s.nonEmpty && s.forall(_.path != real)).label(s"Excludes($real)")
  }
  def includesOnly(path: Path): Seq[FileEvent[_]] => Boolean = {
    val real = realPath(path)
    ((s: Seq[FileEvent[_]]) => s.nonEmpty && s.forall(_.path == real)).label(s"IncludesOnly($real)")
  }
  def isDeletion(path: Path): FileEvent[_] => Boolean = {
    val real = realPath(path)
    (_: FileEvent[_]) match {
      case Deletion(p, _) if p == real => true
      case _                           => false
    }
  }
  def hasDeletion(path: Path): Seq[FileEvent[_]] => Boolean = {
    val real = realPath(path)
    val deletion = isDeletion(real)
    ((_: Seq[FileEvent[_]]).exists(deletion)).label(s"HasDeletion($real)")
  }

  trait Logger extends WatchLogger
  object NullLogger extends Logger { override def debug(msg: Any): Unit = {} }
  // This can't be defined in MonitorOps because of a bug in the scala 2.10 compiler
  @tailrec
  private def drain(monitor: FileEventMonitor[Event],
                    duration: FiniteDuration,
                    events: Seq[Event]): Seq[Event] = {
    val newEvents = monitor.poll(duration)
    if (newEvents.isEmpty) events else drain(monitor, duration, events ++ newEvents)
  }
  implicit class MonitorOps(val monitor: FileEventMonitor[Event]) extends AnyVal {
    def drain(duration: FiniteDuration, events: Seq[Event] = Nil): Seq[Event] =
      EventMonitorSpec.drain(monitor, duration, events)
  }
  implicit class FileOps(val file: File) extends AnyVal {
    def scalaSourceGlobs: Seq[Glob] = {
      val filter = new ExtensionFilter("scala") -- HiddenFileFilter -- new SimpleFilter(
        _.startsWith("."))
      Seq(file.toPath.toRealPath().toFile ** filter)
    }
  }
  class CachingWatchLogger extends Logger {
    val lines = new scala.collection.mutable.ArrayBuffer[String]
    override def debug(msg: Any): Unit = lines.synchronized { lines += msg.toString; () }
    def printLines(msg: String): Unit = println(s"$msg. Log lines:\n${lines mkString "\n"}")
  }
  implicit class ObservableOps(val observable: Observable[Event] with Registerable[Event])
      extends AnyVal {
    def register(globs: Seq[Glob]): Observable[Event] = {
      val delegate = aggregate(globs.flatMap(observable.register(_).toOption): _*)
      new Observable[Event] {
        override def addObserver(o: Observer[Event]): AutoCloseable = delegate.addObserver(o)
        override def close(): Unit = {
          delegate.close()
          observable.close()
        }
      }
    }
  }
  def aggregate[T](observables: Observable[T]*): Observable[T] = {
    val observers = new Observers[T]
    val handles = observables.map(observers.addObservable)
    new Observable[T] {
      override def addObserver(observer: Observer[T]): AutoCloseable =
        observers.addObserver(observer)
      override def close(): Unit = {
        handles.foreach(_.close())
      }
    }
  }
}

private[sbt] trait RepoEventMonitorSpec extends FlatSpec with Matchers with EventMonitorSpec {
  val converter: (Path, FileAttributes) => Try[Unit] =
    (_: Path, _: FileAttributes) => Success(())
  private[sbt] def factory(): FileTreeRepository[FileAttributes]
  override def newObservable(globs: Seq[Glob], logger: Logger): Observable[Event] = {
    val repository = factory()
    new Observable[Event] {
      val aggregated =
        EventMonitorSpec.aggregate(globs.flatMap(repository.register(_).toOption): _*)
      override def addObserver(observer: Observer[Event]): AutoCloseable =
        aggregated.addObserver(observer)
      override def close(): Unit = {
        aggregated.close()
        repository.close()
      }
    }
  }
}
class FileTreeRepositoryEventMonitorSpec extends RepoEventMonitorSpec {
  override def pollDelay: FiniteDuration = 100.millis
  override private[sbt] def factory(): FileTreeRepository[FileAttributes] =
    FileTreeRepository.default
}

class LegacyFileTreeRepositoryEventMonitorSpec extends RepoEventMonitorSpec {
  override def pollDelay: FiniteDuration = 100.millis
  override private[sbt] def factory(): FileTreeRepository[FileAttributes] =
    FileTreeRepository.legacy
}

private[sbt] abstract class SourceModificationWatchSpec(
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

  override def newObservable(globs: Seq[Glob], logger: Logger): Observable[Event] = {
    val watchState = WatchState.empty(globs, getService)
    val observable =
      new WatchServiceBackedObservable(watchState, 5.millis, closeService = true, logger = logger)
    observable.register(globs)
  }
}
