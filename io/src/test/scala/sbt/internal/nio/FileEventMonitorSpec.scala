/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.nio

import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sbt.internal.nio.FileEvent.{ Creation, Deletion, Update }
import sbt.nio.file.FileAttributes

import scala.concurrent.duration.{ Deadline => _, _ }

class FileEventMonitorSpec extends AnyFlatSpec with Matchers {
  import FileEventMonitorSpec._
  private[nio] def antiEntropyMonitor[T <: FileAttributes](
      observable: Observable[FileEvent[T]],
      period: FiniteDuration,
      logger: WatchLogger
  )(implicit timeSource: TimeSource): FileEventMonitor[FileEvent[T]] =
    FileEventMonitor.antiEntropy(observable, period, logger, 50.millis, 10.minutes)
  object TestAttributes {
    def apply(
        isDirectory: Boolean = false,
        isRegularFile: Boolean = false,
        isSymbolicLink: Boolean = false
    ): FileAttributes =
      FileAttributes(isDirectory, isOther = false, isRegularFile, isSymbolicLink)
  }
  class DeterminsticTimeSource extends TimeSource with AutoCloseable {
    private val currentTime = new AtomicReference[FiniteDuration](System.currentTimeMillis.millis)
    private val timeoutThread = new Thread {
      setDaemon(true)
      start()
      override def run(): Unit = {
        try {
          Thread.sleep(5.seconds.toMillis)
          increment(5.minutes)
        } catch { case _: InterruptedException => } // drop interrupts since they come from close.
      }
    }
    override def close(): Unit = {
      timeoutThread.interrupt()
      timeoutThread.join(500)
    }
    override def now: Deadline = new DeadlineImpl()
    def increment(duration: FiniteDuration): Unit = {
      val current = currentTime.get()
      currentTime.set(current + duration)
    }
    def incrementAsync(duration: FiniteDuration): Unit = {
      new Thread("increment-deadline-source") {
        setDaemon(true)
        start()
        override def run(): Unit = {
          Thread.sleep(2)
          increment(duration)
        }
      }
      ()
    }
    private class DeadlineImpl(override val value: FiniteDuration) extends Deadline {
      def this() = this(currentTime.get())
      override def isOverdue: Boolean = {
        val now = currentTime.get()
        now > value
      }
      override def +(duration: FiniteDuration): Deadline = new DeadlineImpl(value + duration)
      override def -(duration: FiniteDuration): Deadline = new DeadlineImpl(value - duration)
    }
  }
  def withDeterministicSource[R](f: DeterminsticTimeSource => R): R = {
    val deadlineSource = new DeterminsticTimeSource
    try f(deadlineSource)
    finally deadlineSource.close()
  }
  "anti-entropy" should "ignore redundant events" in withDeterministicSource { implicit source =>
    val observers = new Observers[Event]
    val antiEntropyPeriod = 20.millis
    val monitor = antiEntropyMonitor(observers, antiEntropyPeriod, NullWatchLogger)
    val foo = Paths.get("foo")
    val startAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, startAttributes)
    observers.onNext(fooCreation)
    observers.onNext(Update(foo, startAttributes, startAttributes))
    val barAttributes = TestAttributes(isRegularFile = true)
    val bar = Paths.get("bar")
    val barCreation = Creation(bar, barAttributes)
    observers.onNext(barCreation)
    source.incrementAsync(antiEntropyPeriod + 5.millis)
    assert(monitor.poll(antiEntropyPeriod).toSet[Event] == Set(fooCreation, barCreation))
    val wait = antiEntropyPeriod + 100.millis
    source.incrementAsync(wait + 5.millis)
    monitor.poll(wait) shouldBe Nil
    val update = Update(foo, startAttributes, startAttributes)
    observers.onNext(update)
    assert(monitor.poll(antiEntropyPeriod) == Seq(update))
  }
  it should "not ignore new events" in withDeterministicSource { implicit source =>
    val observers = new Observers[Event]
    val antiEntropyPeriod = 20.millis
    val monitor =
      antiEntropyMonitor(observers, antiEntropyPeriod, NullWatchLogger)
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
    assert(monitor.poll(antiEntropyPeriod).toSet[Event] == Set(fooCreation, barCreation))
    new Thread("anti-entropy-test") {
      setDaemon(true)
      start()
      override def run(): Unit = {
        source.increment(2 * antiEntropyPeriod)
        observers.onNext(Update(foo, fooAttributes, fooAttributes, Deadline.now))
        source.increment(10.seconds)
      }
    }
    // Ensure the timeout is long enough for the background thread to call onUpdate
    assert(monitor.poll(5.seconds) == Seq(fooUpdate))
  }
  private def deletionSpec(finite: Boolean): Unit = withDeterministicSource { implicit source =>
    val observers = new Observers[Event]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(
        observers,
        antiEntropyPeriod,
        NullWatchLogger,
        quarantinePeriod,
        10.minutes
      )
    val foo = Paths.get("foo")
    val fooAttributes = FileAttributes.NonExistent
    val fooDeletion = Deletion(foo, fooAttributes, Deadline.now)
    observers.onNext(fooDeletion)
    monitor.poll(0.millis) shouldBe Nil
    source.incrementAsync(quarantinePeriod * 3)
    val period = if (finite) quarantinePeriod * 2 else Duration.Inf
    assert(monitor.poll(period) == Seq(fooDeletion))
    ()
  }
  it should "quarantine deletions with finite poll" in deletionSpec(finite = true)
  it should "quarantine deletions with infinite poll" in deletionSpec(finite = false)
  it should "immediately trigger for creations" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(
        observers,
        antiEntropyPeriod,
        NullWatchLogger,
        quarantinePeriod,
        10.minutes
      )
    val foo = Paths.get("foo")
    val deletionAttributes = FileAttributes.NonExistent
    val fooDeletion = Deletion(foo, deletionAttributes)
    val creationAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, creationAttributes)
    observers.onNext(fooDeletion)
    observers.onNext(fooCreation)

    assert(monitor.poll(0.millis) == Seq(Update(foo, deletionAttributes, creationAttributes)))
  }
  it should "immediately trigger with zero antientropy" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 0.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(
        observers,
        antiEntropyPeriod,
        NullWatchLogger,
        quarantinePeriod,
        10.minutes
      )
    val foo = Paths.get("foo")
    val updateAttributes = TestAttributes(isRegularFile = true)
    val fooUpdate = Update(foo, updateAttributes, updateAttributes)
    observers.onNext(fooUpdate)
    assert(monitor.poll(0.millis) == Seq(Update(foo, updateAttributes, updateAttributes)))
    observers.onNext(fooUpdate)
    assert(monitor.poll(0.millis) == Seq(Update(foo, updateAttributes, updateAttributes)))

  }
}
object FileEventMonitorSpec extends Matchers {
  private type Event = FileEvent[FileAttributes]
}
