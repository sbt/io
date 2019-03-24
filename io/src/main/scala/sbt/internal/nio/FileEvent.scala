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

import java.nio.file.Path

private[sbt] sealed trait FileEvent[+T] {
  import FileEvent._
  def path: Path
  def attributes: T
  def exists: Boolean
  def occurredAt: Deadline
  private[sbt] def map[U](f: T => U): FileEvent[U] = this match {
    case c: Creation[T] => Creation(path, f(c.attributes), c.occurredAt)
    case d: Deletion[T] => Deletion(path, f(d.attributes), d.occurredAt)
    case u: Update[T]   => Update(path, f(u.previousAttributes), f(u.attributes), u.occurredAt)
  }
}
private[sbt] object FileEvent {
  def unapply[T](event: FileEvent[T]): Option[(Path, T)] =
    event match {
      case Creation(path, attributes)  => Some((path, attributes))
      case Deletion(path, attributes)  => Some((path, attributes))
      case Update(path, _, attributes) => Some((path, attributes))
    }
  private[sbt] abstract case class Creation[+T] private[FileEvent] (override val path: Path,
                                                                    attributes: T)
      extends FileEvent[T] {
    override def exists: Boolean = true
  }
  private[sbt] object Creation {
    def apply[T](path: Path, attributes: T)(implicit timeSource: TimeSource): Creation[T] =
      new Creation(path, attributes) { override val occurredAt: Deadline = timeSource.now }
    def apply[T](path: Path, attributes: T, deadline: Deadline): Creation[T] =
      new Creation(path, attributes) { override val occurredAt: Deadline = deadline }
  }
  private[sbt] abstract case class Update[+T] private[FileEvent] (override val path: Path,
                                                                  previousAttributes: T,
                                                                  attributes: T)
      extends FileEvent[T] {
    override def exists: Boolean = true
  }
  private[sbt] object Update {
    def apply[T](path: Path, previousAttributes: T, attributes: T)(
        implicit timeSource: TimeSource): Update[T] =
      new Update(path, previousAttributes, attributes) {
        override val occurredAt: Deadline = timeSource.now
      }
    def apply[T](path: Path, previousAttributes: T, attributes: T, deadline: Deadline): Update[T] =
      new Update(path, previousAttributes, attributes) {
        override val occurredAt: Deadline = deadline
      }
  }
  private[sbt] abstract case class Deletion[+T] private[FileEvent] (override val path: Path,
                                                                    override val attributes: T)
      extends FileEvent[T] {
    override def exists: Boolean = false
  }
  private[sbt] object Deletion {
    def apply[T](path: Path, attributes: T)(implicit timeSource: TimeSource): Deletion[T] =
      new Deletion(path, attributes) { override val occurredAt: Deadline = timeSource.now }
    def apply[T](path: Path, attributes: T, deadline: Deadline): Deletion[T] =
      new Deletion(path, attributes) { override val occurredAt: Deadline = deadline }
  }

}
