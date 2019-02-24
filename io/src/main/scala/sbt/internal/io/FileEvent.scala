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

private[sbt] sealed trait FileEvent[+T] {
  def path: NioPath
  def attributes: T
  def occurredAt: Deadline
}
private[sbt] object FileEvent {
  def unapply[T](event: FileEvent[T]): Option[(NioPath, T)] =
    event match {
      case Creation(path, attributes)  => Some((path, attributes))
      case Deletion(path, attributes)  => Some((path, attributes))
      case Update(path, _, attributes) => Some((path, attributes))
    }
  private[sbt] abstract case class Creation[+T] private[FileEvent] (override val path: NioPath,
                                                                    attributes: T)
      extends FileEvent[T]
  private[sbt] object Creation {
    def apply[T](path: NioPath, attributes: T)(implicit timeSource: TimeSource): Creation[T] =
      new Creation(path, attributes) { override val occurredAt: Deadline = timeSource.now }
    def apply[T](path: NioPath, attributes: T, deadline: Deadline): Creation[T] =
      new Creation(path, attributes) { override val occurredAt: Deadline = deadline }
  }
  private[sbt] abstract case class Update[+T] private[FileEvent] (override val path: NioPath,
                                                                  previousAttributes: T,
                                                                  attributes: T)
      extends FileEvent[T]
  private[sbt] object Update {
    def apply[T](path: NioPath, previousAttributes: T, attributes: T)(
        implicit timeSource: TimeSource): Update[T] =
      new Update(path, previousAttributes, attributes) {
        override val occurredAt: Deadline = timeSource.now
      }
    def apply[T](path: NioPath,
                 previousAttributes: T,
                 attributes: T,
                 deadline: Deadline): Update[T] =
      new Update(path, previousAttributes, attributes) {
        override val occurredAt: Deadline = deadline
      }
  }
  private[sbt] abstract case class Deletion[+T] private[FileEvent] (override val path: NioPath,
                                                                    override val attributes: T)
      extends FileEvent[T]
  private[sbt] object Deletion {
    def apply[T](path: NioPath, attributes: T)(implicit timeSource: TimeSource): Deletion[T] =
      new Deletion(path, attributes) { override val occurredAt: Deadline = timeSource.now }
    def apply[T](path: NioPath, attributes: T, deadline: Deadline): Deletion[T] =
      new Deletion(path, attributes) { override val occurredAt: Deadline = deadline }
  }

}
