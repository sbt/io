package sbt.internal.io

import java.nio.file.{ ClosedWatchServiceException, Paths }

import org.scalatest.{ Assertion, FlatSpec, Matchers }
import sbt.io.syntax._
import sbt.io.{ IO, SimpleFilter, WatchService }

import scala.concurrent.duration._

abstract class SourceModificationWatchSpec(
    getService: => WatchService,
    pollDelay: FiniteDuration
) extends FlatSpec
    with Matchers {
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
      val monitor = defaultMonitor(getService, parentDir, tc = tc)
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

      val monitor = defaultMonitor(getService, parentDir)
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
      val monitor = defaultMonitor(getService, parentDir, antiEntropy = maxWait * 2)
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
      val monitor = addMonitor(WatchState.empty(getService, Seq(source)), 0.seconds, tc = tc())
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
      override def debug(msg: => Any): Unit = {
        lines :+= msg.toString
      }
    }
    val tc = defaultTerminationCondition
    val monitor =
      EventMonitor(WatchState.empty(getService, sources), pollDelay, 0.seconds, tc(), logger)
    try {
      val triggered = watchTest(monitor) {
        IO.write(file, "bar")
      }
      assert(triggered)
      assert(monitor.state().count == 2)
      assert(lines.exists(_.startsWith("Triggered")))
    } finally monitor.close()
  }

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

  private def watchTest(eventMonitor: EventMonitor)(modifier: => Unit): Boolean = {
    modifier
    eventMonitor.awaitEvent()
  }

  private def watchTest(base: File, expectedTrigger: Boolean = true)(
      modifier: => Unit): Assertion = {
    val monitor = defaultMonitor(getService, base)
    try {
      val triggered = watchTest(monitor)(modifier)
      triggered shouldBe expectedTrigger
    } finally monitor.close()
  }

  private def defaultTerminationCondition: () => Boolean = {
    lazy val deadline = maxWait.fromNow
    () =>
      {
        val res = deadline.isOverdue()
        if (!res) Thread.sleep(5)
        res
      }
  }
  private def addMonitor(s: WatchState,
                         antiEntropy: FiniteDuration,
                         tc: => Boolean): EventMonitor = {
    EventMonitor(s, pollDelay, antiEntropy, tc)
  }
  private def defaultMonitor(service: WatchService,
                             base: File,
                             antiEntropy: FiniteDuration = 0.milliseconds,
                             tc: () => Boolean = defaultTerminationCondition): EventMonitor = {
    val sources = Seq(
      Source(base.toPath.toRealPath().toFile, "*.scala", new SimpleFilter(_.startsWith("."))))
    addMonitor(WatchState.empty(service, sources), antiEntropy, tc())
  }

  private def writeNewFile(file: File, content: String): Unit = {
    IO.write(file, content)
    IO.setModifiedTimeOrFalse(file, (Deadline.now - 5.seconds).timeLeft.toMillis)
    ()
  }
}
