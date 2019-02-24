package sbt.internal.io
import scala.concurrent.duration._

/**
 * A factory that generates instances of [[Deadline]]. This will typically just get the current
 * time using System.currentTimeMillis but in testing can be replaced with a mutable time source
 * that the the test author can manually increment to verify certain behaviors. Without this
 * indirection, it is difficult to make the tests deterministic.
 */
private[io] trait TimeSource {
  def now: Deadline
}
private[io] object TimeSource {
  implicit object default extends TimeSource {
    override def now: Deadline = new DefaultImpl()
  }

  /**
   * Use System.currentTimeMillis because we don't really care that much about precision and
   * System.currentTimeMillis has far less overhead than System.nanoTime.
   * @param value wraps a FiniteDuration which represents the duration since the epoch.
   */
  private class DefaultImpl(override val value: FiniteDuration) extends Deadline {
    def this() = this(System.currentTimeMillis.millis)
    override def isOverdue: Boolean = System.currentTimeMillis > value.toMillis
    override def +(duration: FiniteDuration): Deadline = new DefaultImpl(value + duration)
    override def -(duration: FiniteDuration): Deadline = new DefaultImpl(value - duration)
  }
}

/**
 * Mirrors a subset of the scala.concurrent.duration.Deadline api. The motivation is to allow for
 * testing where we want to deterministically control the how the test clock evolves over time.
 */
private[io] trait Deadline extends Comparable[Deadline] {
  def isOverdue: Boolean
  def value: Duration
  def +(duration: FiniteDuration): Deadline
  def -(duration: FiniteDuration): Deadline
  final def -(deadline: Deadline): Duration = this.value match {
    case fd: FiniteDuration =>
      deadline.value match {
        case tfd: FiniteDuration => fd - tfd
        case _                   => Duration.MinusInf
      }
    case d => d
  }
  final def <(that: Deadline): Boolean = this.compareTo(that) < 0
  final def <=(that: Deadline): Boolean = this.compareTo(that) <= 0
  final def >(that: Deadline): Boolean = this.compareTo(that) > 0
  final def >=(that: Deadline): Boolean = this.compareTo(that) >= 0
  override def compareTo(that: Deadline): Int = this.value compareTo that.value
}
private[io] object Deadline {
  def now(implicit timeSource: TimeSource): Deadline = timeSource.now
  private[io] object Inf extends Deadline {
    override val value = Duration.Inf
    override def isOverdue: Boolean = false
    override def compareTo(o: Deadline): Int = this.value compareTo o.value
    override def +(duration: FiniteDuration): Deadline = this
    override def -(duration: FiniteDuration): Deadline = this
  }
}
