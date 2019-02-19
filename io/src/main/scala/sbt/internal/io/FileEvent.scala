/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.io

import java.nio.file.{ Path => NioPath }

import scala.concurrent.duration.Deadline

private[sbt] sealed trait FileEvent[+T] {
  def path: NioPath
  def attributes: T
  def occurredAt: Deadline
}
private[sbt] object FileEvent {
  private[sbt] def observer[T](
      onCreate: (NioPath, T) => Unit,
      onDelete: (NioPath, T) => Unit,
      onUpdate: ((NioPath, T), (NioPath, T)) => Unit): Observer[(NioPath, FileEvent[T])] =
    new Observer[(NioPath, FileEvent[T])] {
      override def onNext(t: (NioPath, FileEvent[T])): Unit = t match {
        case (_, c: Creation[T]) => onCreate(c.path, c.attributes)
        case (_, d: Deletion[T]) => onDelete(d.path, d.attributes)
        case (_, u: Update[T])   => onUpdate(u.path -> u.previousAttributes, u.path -> u.attributes)
      }
    }
  def unapply[T](event: FileEvent[T]): Option[(NioPath, T, Deadline)] =
    event match {
      case Creation(path, attributes, occurredAt)  => Some((path, attributes, occurredAt))
      case Deletion(path, attributes, occurredAt)  => Some((path, attributes, occurredAt))
      case Update(path, _, attributes, occurredAt) => Some((path, attributes, occurredAt))
    }
  private[sbt] final case class Creation[+T](override val path: NioPath,
                                             attributes: T,
                                             override val occurredAt: Deadline = Deadline.now)
      extends FileEvent[T]
  private[sbt] final case class Update[+T](override val path: NioPath,
                                           previousAttributes: T,
                                           attributes: T,
                                           occurredAt: Deadline = Deadline.now)
      extends FileEvent[T]
  private[sbt] final case class Deletion[+T](override val path: NioPath,
                                             override val attributes: T,
                                             occurredAt: Deadline = Deadline.now)
      extends FileEvent[T]

}
