package sbt.internal.io

import java.nio.file.{ Files, Path }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.internal.io.HybridEventMonitorSpec._
import sbt.io.FileTreeView.AllPass
import sbt.io._
import sbt.io.syntax._

import scala.concurrent.duration._
import scala.util.Success

class HybridEventMonitorSpec extends FlatSpec with Matchers {
  private val timeout = 200.milliseconds
  it should "poll and monitor" in IO.withTemporaryDirectory { baseDir =>
    val dir = baseDir.toPath.toRealPath()
    val pollingDir = Files.createDirectory(dir.resolve("polling"))
    val monitoredDir = Files.createDirectory(dir.resolve("monitored"))
    val converter = (path: Path, attributes: SimpleFileAttributes) =>
      CustomFileAttributes.get(path, attributes, Success(()))
    val repo = FileTreeRepository.hybrid(converter, pollingDir.toFile ** AllPassFilter)
    val globs = Seq(pollingDir.toFile ** AllPassFilter, monitoredDir.toFile ** AllPassFilter)
    globs.foreach(repo.register(_: Glob))
    val pollingFile = pollingDir.resolve("file")
    val monitoredFile = monitoredDir.resolve("file")

    try {
      withMonitor(repo, globs) { monitor =>
        Files.createFile(pollingFile)
        assert(monitor.poll(timeout).nonEmpty)
        repo.ls(pollingDir) shouldBe Seq(pollingFile)
      }

      withMonitor(repo, globs) { monitor =>
        Files.createFile(monitoredFile)
        assert(monitor.poll(timeout).nonEmpty)
        repo.ls(monitoredDir) shouldBe Seq(monitoredFile)
      }

      val newPollingFile = pollingDir.resolve("new-file")
      val newMonitoredFile = monitoredDir.resolve("new-file")
      // This tests that monitoring still works when there is overlap of the registered files
      repo.register(dir ** AllPassFilter)
      withMonitor(repo, globs) { monitor =>
        Files.createFile(newPollingFile)
        assert(monitor.poll(timeout).nonEmpty)
        repo.ls(pollingDir).toSet shouldBe Set(pollingFile, newPollingFile)
      }
      withMonitor(repo, globs) { monitor =>
        Files.createFile(newMonitoredFile)
        assert(monitor.poll(timeout).nonEmpty)
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
    val logger: WatchLogger = NullWatchLogger
    val monitor = observable match {
      case r: HybridPollingFileTreeRepository[T] @unchecked =>
        FileEventMonitor(r.toPollingObservable(pollDelay, logger))
    }
    try f(monitor)
    finally monitor.close()
  }
  implicit class FileRepositoryOps[+T](val fileRepository: FileTreeRepository[T]) {
    def ls(path: Path): Seq[Path] = fileRepository.list(path ** AllPassFilter, AllPass).map(_._1)
  }
}
