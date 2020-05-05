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
import java.io.IOException

import scala.util.control.NonFatal

private[sbt] object Retry {
  private lazy val limit = {
    val defaultLimit = 10
    try System.getProperty("sbt.io.retry.limit", defaultLimit.toString).toInt
    catch { case NonFatal(_) => defaultLimit }
  }
  private[sbt] def apply[@specialized T](f: => T, excludedExceptions: Class[_ <: IOException]*): T =
    apply(f, limit, excludedExceptions: _*)
  private[sbt] def apply[@specialized T](
      f: => T,
      limit: Int,
      excludedExceptions: Class[_ <: IOException]*
  ): T = {
    require(limit >= 1, "limit must be 1 or higher: was: " + limit)
    def filter(e: Exception): Boolean = excludedExceptions match {
      case s if s.nonEmpty =>
        !excludedExceptions.exists(_.isAssignableFrom(e.getClass))
      case _ =>
        true
    }
    var attempt = 1
    var firstException: IOException = null
    while (attempt <= limit) {
      try {
        return f
      } catch {
        case e: IOException if filter(e) =>
          if (firstException == null) firstException = e

          Thread.sleep(0);
          attempt += 1
      }
    }
    throw firstException
  }
}
