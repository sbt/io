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

import scala.annotation.tailrec
import scala.util.control.NonFatal

private[sbt] object Retry {
  private lazy val limit = {
    val defaultLimit = 10
    try System.getProperty("sbt.io.retry.limit", defaultLimit.toString).toInt
    catch { case NonFatal(_) => defaultLimit }
  }
  private[sbt] def apply[T](f: => T, excludedExceptions: Class[_ <: IOException]*): T =
    apply(f, limit, excludedExceptions: _*)
  private[sbt] def apply[T](f: => T,
                            limit: Int,
                            excludedExceptions: Class[_ <: IOException]*): T = {
    lazy val filter: Exception => Boolean = excludedExceptions match {
      case s if s.nonEmpty =>
        (e: Exception) =>
          !excludedExceptions.exists(_.isAssignableFrom(e.getClass))
      case _ =>
        (_: Exception) =>
          true
    }
    @tailrec
    def impl(attempt: Int): T = {
      val (retry, res) = try false -> Right(f)
      catch {
        case e: IOException if filter(e) && (attempt < limit) => (true, Left(e))
        case e: IOException                                   => (false, Left(e))
      }
      if (retry) { Thread.sleep(0); impl(attempt + 1) } else {
        res match {
          case Right(r) => r
          case Left(e)  => throw e
        }
      }
    }
    impl(1)
  }
}
