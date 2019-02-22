/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.io

private[sbt] trait WatchLogger {
  def debug(msg: => Any): Unit
}
private[sbt] object NullWatchLogger extends WatchLogger {
  private def ignoreArg[T](f: => T): Unit = if (false) { f; () } else ()
  override def debug(msg: => Any): Unit = ignoreArg(msg)
}
