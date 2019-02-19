package sbt.internal.io

import java.nio.file.Paths

import org.scalatest.{ FlatSpec, Matchers }
import sbt.internal.io.FileEvent.{ Creation, Deletion, Update }
import sbt.io._

import scala.concurrent.duration._

class FileEventMonitorSpec extends FlatSpec with Matchers {
  import FileEventMonitorSpec._
  private[sbt] def antiEntropyMonitor[T <: SimpleFileAttributes](
      observable: Observable[FileEvent[T]],
      period: FiniteDuration,
      logger: WatchLogger): FileEventMonitor[FileEvent[T]] =
    FileEventMonitor.antiEntropy(observable, period, logger, 50.millis, 10.minutes)
  object TestAttributes {
    def apply(exists: Boolean = true,
              isDirectory: Boolean = false,
              isRegularFile: Boolean = false,
              isSymbolicLink: Boolean = false): SimpleFileAttributes =
      SimpleFileAttributes.get(exists, isDirectory, isRegularFile, isSymbolicLink)
  }
  "anti-entropy" should "in redundant events" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 20.millis
    val monitor = antiEntropyMonitor(observers, antiEntropyPeriod, NullWatchLogger)
    val start = Deadline.now
    val foo = Paths.get("foo")
    val startAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, startAttributes)
    observers.onNext(fooCreation)
    observers.onNext(Update(foo, startAttributes, startAttributes))
    val barAttributes = TestAttributes(isRegularFile = true)
    val bar = Paths.get("bar")
    val barCreation = Creation(bar, barAttributes)
    observers.onNext(barCreation)
    monitor.poll(antiEntropyPeriod).toSet[Event] compare Set(fooCreation, barCreation)
    val wait = start + antiEntropyPeriod + 100.millis - Deadline.now
    monitor.poll(wait) shouldBe Nil
    val update = Update(foo, startAttributes, startAttributes)
    observers.onNext(update)
    monitor.poll(antiEntropyPeriod) compare Seq(update)
  }
  it should "not in new events" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 20.millis
    val monitor = antiEntropyMonitor(observers, antiEntropyPeriod, NullWatchLogger)
    val foo = Paths.get("foo")
    val fooAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, fooAttributes)
    val fooUpdate = Update(foo, fooAttributes, fooAttributes)
    observers.onNext(fooCreation)
    observers.onNext(Update(foo, fooAttributes, fooAttributes))
    val bar = Paths.get("bar")
    val barAttributes = TestAttributes(isRegularFile = true)
    val barCreation = Creation(bar, barAttributes)
    observers.onNext(barCreation)
    monitor.poll(antiEntropyPeriod).toSet[Event] compare Set(fooCreation, barCreation)
    val thread = new Thread("anti-entropy-test") {
      override def run(): Unit = {
        Thread.sleep(2 * antiEntropyPeriod.toMillis)
        observers.onNext(Update(foo, fooAttributes, fooAttributes))
      }
    }
    thread.setDaemon(true)
    thread.start()
    // Ensure the timeout is long enough for the background thread to call onUpdate
    monitor.poll(5.seconds) compare Seq(fooUpdate)
  }
  it should "quarantine deletions" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(observers,
                                   antiEntropyPeriod,
                                   NullWatchLogger,
                                   quarantinePeriod,
                                   10.minutes)
    val foo = Paths.get("foo")
    val fooAttributes = TestAttributes(exists = false, isRegularFile = true)
    val fooDeletion = Deletion(foo, fooAttributes)
    observers.onNext(fooDeletion)
    monitor.poll(0.millis) shouldBe Nil
    monitor.poll(quarantinePeriod * 2) shouldBe Seq(fooDeletion)
  }
  it should "immediately trigger for creations" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(observers,
                                   antiEntropyPeriod,
                                   NullWatchLogger,
                                   quarantinePeriod,
                                   10.minutes)
    val foo = Paths.get("foo")
    val deletionAttributes = TestAttributes(exists = false, isRegularFile = true)
    val fooDeletion = Deletion(foo, deletionAttributes)
    val creationAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, creationAttributes)
    observers.onNext(fooDeletion)
    observers.onNext(fooCreation)

    monitor.poll(0.millis) compare Seq(Update(foo, deletionAttributes, creationAttributes))
  }
}
object FileEventMonitorSpec extends Matchers {
  private type Event = FileEvent[SimpleFileAttributes]
  private val now = Deadline.now
  private implicit class FileEventOps(val fileEvent: Event) extends AnyVal {
    def stripOccurredAt: FileEvent[SimpleFileAttributes] = fileEvent match {
      case Creation(path, attributes, _)         => Creation(path, attributes, now)
      case Deletion(path, attributes, _)         => Deletion(path, attributes, now)
      case Update(path, previous, attributes, _) => Update(path, previous, attributes, now)
    }
  }
  private implicit class TraversableEventOps[T <: Traversable[Event]](val t: T) extends AnyVal {
    def compare[S <: Traversable[Event]](that: S): Unit = {
      val left = t.map(_.stripOccurredAt)
      val right = that.map(_.stripOccurredAt)
      assert(left == right)
      ()
    }
  }
}
