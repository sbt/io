package sbt.internal.io

import java.nio.file.{ Path, Paths }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.{ FileEventMonitor, NullLogger }
import sbt.io.FileEventMonitor.{ Creation, Deletion, Update }
import sbt.io.FileTreeDataView.Entry

import scala.concurrent.duration._

class FileEventMonitorSpec extends FlatSpec with Matchers {
  object TestEntry {
    val EXISTS = 1
    val DIRECTORY = 2
    val FILE = 4
    val LINK = 8
  }
  import TestEntry._
  class TestEntry(override val getPath: Path, kind: Int)
      extends Entry[Path](getPath, Right(getPath)) {
    override def exists: Boolean = (kind & EXISTS) != 0
    override def isDirectory: Boolean = (kind & DIRECTORY) != 0
    override def isFile: Boolean = (kind & FILE) != 0
    override def isSymbolicLink: Boolean = (kind & LINK) != 0
    override def toRealPath: Path = path
  }
  "anti-entropy" should "ignore redundant events" in {
    val observers = new Observers[Path]
    val antiEntropyPeriod = 20.millis
    val monitor = FileEventMonitor.antiEntropy(observers, antiEntropyPeriod, NullLogger)
    val entry = new TestEntry(Paths.get("foo"), FILE | EXISTS)
    val start = Deadline.now
    observers.onCreate(entry)
    observers.onUpdate(entry, entry)
    val unrelatedEntry = new TestEntry(Paths.get("bar"), FILE | EXISTS)
    observers.onCreate(unrelatedEntry)
    monitor.poll(antiEntropyPeriod).toSet shouldBe Set(new Creation(entry),
                                                       new Creation(unrelatedEntry))
    val wait = start + antiEntropyPeriod + 100.millis - Deadline.now
    monitor.poll(wait) shouldBe Nil
    observers.onUpdate(entry, entry)
    monitor.poll(antiEntropyPeriod) shouldBe Seq(new Update(entry, entry))
  }
  it should "quarantine deletions" in {
    val observers = new Observers[Path]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(observers, antiEntropyPeriod, NullLogger, quarantinePeriod)
    val entry = new TestEntry(Paths.get("foo"), FILE)
    observers.onDelete(entry)
    monitor.poll(0.millis) shouldBe Nil
    monitor.poll(quarantinePeriod * 2) shouldBe Seq(new Deletion(entry))
  }
  it should "immediately trigger for creations" in {
    val observers = new Observers[Path]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(observers, antiEntropyPeriod, NullLogger, quarantinePeriod)
    val deletedEntry = new TestEntry(Paths.get("foo"), FILE)
    val newEntry = new TestEntry(Paths.get("foo"), FILE | EXISTS)
    observers.onDelete(deletedEntry)
    observers.onCreate(newEntry)

    monitor.poll(0.millis) shouldBe Seq(new Update(deletedEntry, newEntry))
  }
}
