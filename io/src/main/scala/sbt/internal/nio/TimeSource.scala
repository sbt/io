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

import scala.concurrent.duration._

/**
 * A factory that generates instances of [[Deadline]]. This will typically just get the current
 * time using System.currentTimeMillis but in testing can be replaced with a mutable time source
 * that the the test author can manually increment to verify certain behaviors. Without this
 * indirection, it is difficult to make the tests deterministic.
 */
private[nio] trait TimeSource {
  def now: Deadline
}
private[nio] object TimeSource {
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
private[nio] trait Deadline extends Comparable[Deadline] {
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
private[nio] object Deadline {
  def now(implicit timeSource: TimeSource): Deadline = timeSource.now
  private[nio] object Inf extends Deadline {
    override val value = Duration.Inf
    override def isOverdue: Boolean = false
    override def compareTo(o: Deadline): Int = this.value compareTo o.value
    override def +(duration: FiniteDuration): Deadline = this
    override def -(duration: FiniteDuration): Deadline = this
  }
}
