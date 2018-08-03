package sbt.internal.io

import java.io.IOException
import java.nio.file.{ ClosedWatchServiceException, Files, Paths }

import org.scalatest.{ Assertion, FlatSpec, Matchers }
import sbt.internal.io.EventMonitor.{ Logger, NullLogger }
import sbt.io.syntax._
import sbt.io.{ IO, SimpleFilter, WatchService }

import scala.annotation.tailrec
import scala.concurrent.duration._

private[sbt] trait EventMonitorSpec { self: FlatSpec with Matchers =>
  def pollDelay: FiniteDuration
  def newEventMonitor(sources: Seq[Source],
                      antiEntropy: FiniteDuration = 0.milliseconds,
                      tc: () => Boolean = defaultTerminationCondition,
                      logger: Logger = NullLogger): EventMonitor
  val maxWait = 2 * pollDelay

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
      val firstDeadline = maxWait.fromNow
      val secondDeadline = (2 * maxWait).fromNow
      var firstDeadlinePassed = false

      val tc = () => {
        if (!firstDeadlinePassed) {
          firstDeadlinePassed = firstDeadline.isOverdue()
          firstDeadlinePassed
        } else {
          secondDeadline.isOverdue()
        }
      }
      val monitor = defaultMonitor(parentDir, tc = tc)
      try {
        val triggered0 = watchTest(monitor) {
          IO.createDirectory(subDir)
        }
        triggered0 shouldBe false
        monitor.state().count shouldBe 1

        val triggered1 = watchTest(monitor) {
          IO.delete(subDir)
        }
        triggered1 shouldBe false
        monitor.state().count shouldBe 1
      } finally monitor.close()
  }

  it should "detect deletion of a directory containing watched files" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val src = subDir / "src.scala"

      IO.createDirectory(parentDir)

      val monitor = defaultMonitor(parentDir)
      try {
        val triggered0 = watchTest(monitor) {
          IO.createDirectory(subDir)
          IO.touch(src)
        }
        triggered0 shouldBe true
        monitor.state().count shouldBe 2

        val triggered1 = watchTest(monitor) {
          IO.delete(subDir)
        }
        triggered1 shouldBe true
        monitor.state().count shouldBe 3
      } finally monitor.close()
  }

  it should "not generate multiple events for the same file within anti-entropy period" in IO
    .withTemporaryDirectory { dir =>
      val parentDir = dir / "src" / "watchme"
      val file = parentDir / "Foo.scala"

      writeNewFile(file, "foo")
      val monitor = defaultMonitor(parentDir, antiEntropy = maxWait * 2)
      try {
        val triggered0 = watchTest(monitor) {
          IO.write(file, "bar")
        }
        assert(triggered0)
        assert(IO.read(file) == "bar")

        val triggered1 = watchTest(monitor) {
          IO.write(file, "baz")
        }
        assert(!triggered1)
        assert(IO.read(file) == "baz")
      } finally monitor.close()
    }

  it should "ignore valid files in non-recursive subdirectories" in IO.withTemporaryDirectory {
    dir =>
      val file = dir / "src" / "Foo.scala"
      val source =
        Source(dir.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith(".")))
          .withRecursive(false)
      val tc = defaultTerminationCondition
      val monitor: EventMonitor = newEventMonitor(Seq(source), 0.millis, tc)
      try {
        val triggered = watchTest(monitor) {
          IO.write(file, "foo")
        }
        assert(!triggered)
        assert(IO.read(file) == "foo")
      } finally monitor.close()
  }

  it should "log triggered files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "Foo.scala"

    writeNewFile(file, "foo")

    val sources = Seq(
      Source(parentDir.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith("."))))
    var lines: Seq[String] = Nil
    val logger = new EventMonitor.Logger {
      override def debug(msg: => Any): Unit = lines.synchronized {
        lines :+= msg.toString
      }
    }
    val tc = defaultTerminationCondition
    val monitor = newEventMonitor(sources, 0.seconds, tc, logger)
    try {
      val triggered = watchTest(monitor) {
        IO.write(file, "bar")
      }
      assert(triggered)
      assert(monitor.state().count == 2)
      assert(lines.exists(_.startsWith("Triggered")))
    } finally monitor.close()
  }

  it should "handle rapid creation of many subdirectories and files" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      Files.createDirectories(parentDir.toPath)
      val subdirCount = 2000
      val subdirFileCount = 4
      var files = Seq.empty[File]

      // Longer timeout because there are many file system operations. This can be very expensive
      // especially in the PollingWatchSpec since both the PollingWatchService and the EventMonitor
      // overflow handler are hammering the file system. To minimize the conflicts, we set a long
      // interval between polls in the PollingWatchService using getServiceWithPollDelay. The
      // timeout was increased from 20.seconds to 40.seconds to address transient failures of
      // this test on Appveyor windows builds.
      val deadline = 40.seconds.fromNow
      val monitor = defaultMonitor(parentDir, tc = () => deadline.isOverdue)
      try {
        val triggered0 = watchTest(monitor) {
          val subdirs =
            (1 to subdirCount).map(i =>
              Files.createDirectories(parentDir.toPath.resolve(s"subdir-$i")))
          files = subdirs.flatMap { subdir =>
            subdir.toFile +: (1 to subdirFileCount).map { j =>
              Files.write(subdir.resolve(s"file-$j.scala"), s"foo".getBytes).toFile
            }
          }
        }
        val lastFile = files.last
        assert(triggered0)
        assert(IO.read(lastFile) == s"foo")

        val triggered1 = watchTest(monitor) {
          IO.write(lastFile, "baz")
        }
        assert(triggered1)
        assert(IO.read(lastFile) == "baz")
      } finally monitor.close()
  }
  def watchTest(eventMonitor: EventMonitor)(modifier: => Unit): Boolean = {
    modifier
    eventMonitor.awaitEvent()
  }

  def watchTest(base: File, expectedTrigger: Boolean = true)(modifier: => Unit): Assertion = {
    val sources = Seq(
      Source(base.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith("."))))
    val monitor = newEventMonitor(sources)
    try {
      val triggered = watchTest(monitor)(modifier)
      triggered shouldBe expectedTrigger
    } finally monitor.close()
  }

  def defaultTerminationCondition: () => Boolean = {
    lazy val deadline = maxWait.fromNow
    () =>
      {
        val res = deadline.isOverdue()
        if (!res) Thread.sleep(5)
        res
      }
  }

  private def defaultMonitor(base: File,
                             antiEntropy: FiniteDuration = 0.milliseconds,
                             tc: () => Boolean = defaultTerminationCondition,
                             logger: Logger = NullLogger): EventMonitor = {
    val sources = Seq(
      Source(base.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith("."))))
    newEventMonitor(sources, antiEntropy, tc, logger)
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

  override def newEventMonitor(sources: Seq[Source],
                               antiEntropy: FiniteDuration,
                               tc: () => Boolean,
                               logger: Logger): EventMonitor = {
    EventMonitor(WatchState.empty(getService, sources), pollDelay, antiEntropy, tc(), logger)
  }
}
