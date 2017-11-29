package sbt.internal.io

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
      if (scala.util.Properties.isMac) {
        // Looks like on HFS+ time stamp granularity is only 1 second
        // https://developer.apple.com/library/content/documentation/FileManagement/Conceptual/APFS_Guide/VolumeFormatComparison/VolumeFormatComparison.html
        // therefore in order to notice a last modified time change you need to have elapsed into
        // the next second. We'll do this by sleeping 1 second.
        Thread.sleep(1000L)
      }
      IO.write(file, "bar")
    }
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

}
