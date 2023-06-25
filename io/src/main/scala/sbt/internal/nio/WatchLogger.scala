/*
 * sbt IO
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt.internal.nio

private[sbt] trait WatchLogger {
  def debug(msg: Any): Unit
}
private[sbt] object NullWatchLogger extends WatchLogger {
  private def ignoreArg[T](f: => T): Unit =
    if (false) {
      f; ()
    } else ()
  override def debug(msg: Any): Unit = ignoreArg(msg)
}
