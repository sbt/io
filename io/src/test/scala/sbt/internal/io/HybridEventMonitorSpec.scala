package sbt.internal.io

import java.nio.file.{ Files, Path }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io._
import syntax._

import scala.concurrent.duration._
import HybridEventMonitorSpec._
import sbt.io.FileTreeDataView.Observable

class HybridEventMonitorSpec extends FlatSpec with Matchers {
  it should "poll and monitor" in IO.withTemporaryDirectory { baseDir =>
    val dir = baseDir.toPath.toRealPath()
    val pollingDir = Files.createDirectory(dir.resolve("polling"))
    val monitoredDir = Files.createDirectory(dir.resolve("monitored"))
    val repo = FileTreeRepository.hybrid((_: TypedPath).toPath, pollingDir.toFile ** AllPassFilter)
    val globs = Seq(pollingDir.toFile ** AllPassFilter, monitoredDir.toFile ** AllPassFilter)
    globs.foreach(repo.register(_: Glob))
    val pollingFile = pollingDir.resolve("file")
    val monitoredFile = monitoredDir.resolve("file")

    try {
      withMonitor(repo, globs) { monitor =>
        Files.createFile(pollingFile)
        assert(monitor.poll(5.seconds).nonEmpty)
        repo.ls(pollingDir) shouldBe Seq(pollingFile)
      }

      withMonitor(repo, globs) { monitor =>
        Files.createFile(monitoredFile)
        assert(monitor.poll(5.seconds).nonEmpty)
        repo.ls(monitoredDir) shouldBe Seq(monitoredFile)
      }

      val newPollingFile = pollingDir.resolve("new-file")
      val newMonitoredFile = monitoredDir.resolve("new-file")
      // This tests that monitoring still works when there is overlap of the registered files
      repo.register(dir, Integer.MAX_VALUE)
      withMonitor(repo, globs) { monitor =>
        Files.createFile(newPollingFile)
        assert(monitor.poll(5.seconds).nonEmpty)
        repo.ls(pollingDir).toSet shouldBe Set(pollingFile, newPollingFile)
      }
      withMonitor(repo, globs) { monitor =>
        Files.createFile(newMonitoredFile)
        assert(monitor.poll(5.seconds).nonEmpty)
        repo.ls(monitoredDir).toSet shouldBe Set(monitoredFile, newMonitoredFile)
      }
      val allPolling = Set(pollingDir, pollingFile, newPollingFile)
      val allMonitored = Set(monitoredDir, monitoredFile, newMonitoredFile)
      repo.ls(dir).toSet shouldBe (allPolling ++ allMonitored)
    } finally {
      repo.close()
    }
  }
}

object HybridEventMonitorSpec {
  val antiEntropy: FiniteDuration = 0.seconds
  val pollDelay: FiniteDuration = 100.millis
  def withMonitor[T](observable: Observable[_], sources: Seq[Glob])(
      f: FileEventMonitor[_] => T): T = {
    val monitor = observable match {
      case r: HybridPollingFileTreeRepository[_] =>
        FileEventMonitor(r.toPollingObservable(pollDelay, sources, NullWatchLogger))
    }
    try {
      f(monitor)
    } finally {
      monitor.close()
    }
  }
  implicit class FileRepositoryOps[+T](val fileRepository: FileTreeRepository[T]) {
    def ls(path: Path): Seq[Path] =
      fileRepository.list(path, Integer.MAX_VALUE, (_: TypedPath) => true).map(_.toPath)
  }
}
