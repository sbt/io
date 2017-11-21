package sbt.internal.io

import java.io.{ File, IOException }
import java.nio.file.{ ClosedWatchServiceException, Paths }

import org.scalatest.{ Assertion, FlatSpec, Matchers }
import sbt.io.syntax._
import sbt.io.{ IO, SimpleFilter, WatchService }

import scala.concurrent.duration._

abstract class SourceModificationWatchSpec(
    getService: => WatchService,
    pollDelay: FiniteDuration,
    maxWait: FiniteDuration
) extends FlatSpec
    with Matchers {

  it should "detect modified files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "Foo.scala"

    IO.write(file, "foo")

    watchTest(parentDir) {
      if (!supportsSubSecondModifiedTime(dir)) {
        // Some filesystems, such as HFS+, only have 1 second time stamp granularity
        // https://developer.apple.com/library/content/documentation/FileManagement/Conceptual/APFS_Guide/VolumeFormatComparison/VolumeFormatComparison.html
        // therefore in order to notice a last modified time change you need to have elapsed into
        // the next second. We'll do this by sleeping 1 second.
        Thread.sleep(1000L)
      }
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
      val service = getService
      IO.createDirectory(parentDir)

      try {
        val initState = emptyState(service, parentDir)
        val (triggered0, newState0) = watchTest(initState) {
          IO.createDirectory(subDir)
        }
        triggered0 shouldBe false
        newState0.count shouldBe 1

        val (triggered1, newState1) = watchTest(newState0) {
          IO.delete(subDir)
        }
        triggered1 shouldBe false
        newState1.count shouldBe 1
      } finally service.close()
  }

  it should "detect deletion of a directory containing watched files" in IO.withTemporaryDirectory {
    dir =>
      val parentDir = dir / "src" / "watchme"
      val subDir = parentDir / "subdir"
      val src = subDir / "src.scala"
      val service = getService

      IO.createDirectory(parentDir)

      try {
        val initState = emptyState(service, parentDir)
        val (triggered0, newState0) = watchTest(initState) {
          IO.createDirectory(subDir)
          IO.touch(src)
        }
        triggered0 shouldBe true
        newState0.count shouldBe 2

        val (triggered1, newState1) = watchTest(newState0) {
          IO.delete(subDir)
        }
        triggered1 shouldBe true
        newState1.count shouldBe 3
      } finally service.close()
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

  private def watchTest(initState: WatchState)(modifier: => Unit): (Boolean, WatchState) = {
    var started = false
    val deadline = maxWait.fromNow
    val modThread = new Thread { override def run() = modifier }
    SourceModificationWatch.watch(pollDelay, initState) {
      if (!started) {
        started = true
        modThread.start()
      }
      deadline.isOverdue()
    }
  }

  private def watchTest(base: File, expectedTrigger: Boolean = true)(
      modifier: => Unit): Assertion = {
    val service = getService
    try {
      val initState = emptyState(service, base)
      val (triggered, _) = watchTest(initState)(modifier)
      triggered shouldBe expectedTrigger
    } finally service.close()
  }

  private def emptyState(service: WatchService, base: File): WatchState = {
    val sources = Seq(Source(base, "*.scala", new SimpleFilter(_.startsWith("."))))
    WatchState.empty(service, sources).withCount(1)
  }

  private def supportsSubSecondModifiedTime(dir: File) = {
    val file = File.createTempFile("sbt", null, dir)
    try {
      val oldTime = IO getModifiedTime file
      IO.setModifiedTime(file, oldTime - 27)
      val newTime = IO getModifiedTime file
      newTime + 27 == oldTime
    } finally {
      if (!file.delete())
        throw new IOException("Failed to delete temp file: " + file.getAbsolutePath)
    }
  }

}
