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

import com.swoval.functional.{ Either => SEither }

/**
 * Utilities for converting between swoval and sbt data types.
 */
private[io] object SwovalConverters {

  implicit class SwovalEitherOps[L, R](val either: SEither[L, R]) extends AnyVal {
    def asScala[R0](implicit f: R => R0): Either[L, R0] = either match {
      case l: com.swoval.functional.Either.Left[L, R] =>
        Left(com.swoval.functional.Either.leftProjection(l).getValue)
      case r: com.swoval.functional.Either.Right[L, R] => Right(f(r.get()))
    }
  }
}
