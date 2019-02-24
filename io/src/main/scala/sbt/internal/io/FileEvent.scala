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
