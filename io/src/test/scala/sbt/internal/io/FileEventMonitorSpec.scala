package sbt.internal.io

import java.nio.file.{ Path, Paths }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.{ FileEventMonitor, NullLogger, TypedPath }
import sbt.io.FileEventMonitor.{ Creation, Deletion, Update }
import sbt.io.FileTreeDataView.Entry

import scala.concurrent.duration._

class FileEventMonitorSpec extends FlatSpec with Matchers {
  object TestEntry {
    val EXISTS = 1
    val DIRECTORY = 2
    val FILE = 4
    val LINK = 8
    def apply(file: String, kind: Int): Entry[Path] = {
      val path = Paths.get(file)
      new Entry(new TestTypedPath(path, kind), Right(path))
    }
  }
  import TestEntry._
  class TestTypedPath(override val getPath: Path, kind: Int) extends TypedPath {
    override def exists: Boolean = (kind & EXISTS) != 0
    override def isDirectory: Boolean = (kind & DIRECTORY) != 0
    override def isFile: Boolean = (kind & FILE) != 0
    override def isSymbolicLink: Boolean = (kind & LINK) != 0
  }
  "anti-entropy" should "ignore redundant events" in {
    val observers = new Observers[Path]
    val antiEntropyPeriod = 20.millis
    val monitor = FileEventMonitor.antiEntropy(observers, antiEntropyPeriod, NullLogger)
    val entry = TestEntry("foo", FILE | EXISTS)
    val start = Deadline.now
    observers.onCreate(entry)
    observers.onUpdate(entry, entry)
    val unrelatedEntry = TestEntry("bar", FILE | EXISTS)
    observers.onCreate(unrelatedEntry)
    monitor.poll(antiEntropyPeriod).toSet shouldBe Set(Creation(entry), Creation(unrelatedEntry))
    val wait = start + antiEntropyPeriod + 100.millis - Deadline.now
    monitor.poll(wait) shouldBe Nil
    observers.onUpdate(entry, entry)
    monitor.poll(antiEntropyPeriod) shouldBe Seq(Update(entry, entry))
  }
  it should "quarantine deletions" in {
    val observers = new Observers[Path]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(observers, antiEntropyPeriod, NullLogger, quarantinePeriod)
    val entry = TestEntry("foo", FILE)
    observers.onDelete(entry)
    monitor.poll(0.millis) shouldBe Nil
    monitor.poll(quarantinePeriod * 2) shouldBe Seq(Deletion(entry))
  }
  it should "immediately trigger for creations" in {
    val observers = new Observers[Path]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(observers, antiEntropyPeriod, NullLogger, quarantinePeriod)
    val deletedEntry = TestEntry("foo", FILE)
    val newEntry = TestEntry("foo", FILE | EXISTS)
    observers.onDelete(deletedEntry)
    observers.onCreate(newEntry)

    monitor.poll(0.millis) shouldBe Seq(Update(deletedEntry, newEntry))
  }
}
