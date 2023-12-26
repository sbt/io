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

package sbt.internal.io
import java.io.IOException

import scala.util.control.NonFatal

private[sbt] object Retry {
  private lazy val limit = {
    val defaultLimit = 10
    try System.getProperty("sbt.io.retry.limit", defaultLimit.toString).toInt
    catch { case NonFatal(_) => defaultLimit }
  }
  private[sbt] def apply[@specialized T](f: => T, excludedExceptions: Class[? <: IOException]*): T =
    apply(f, limit, excludedExceptions: _*)
  private[sbt] def apply[@specialized T](
      f: => T,
      limit: Int,
      excludedExceptions: Class[? <: IOException]*,
  ): T = apply(f, limit, 100, excludedExceptions: _*)
  private[sbt] def apply[@specialized T](
      f: => T,
      limit: Int,
      sleepInMillis: Long,
      excludedExceptions: Class[? <: IOException]*,
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
          // https://github.com/sbt/io/issues/295
          // On Windows, we're seeing java.nio.file.AccessDeniedException with sleep(0).
          Thread.sleep(sleepInMillis)
          attempt += 1
      }
    }
    throw firstException
  }
}
