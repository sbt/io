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
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec

final class RetrySpec extends AnyFlatSpec {
  private val noExcluded: List[Class[_ <: IOException]] = List[Class[_ <: IOException]]()
  "retry" should "throw first exception after number of failures" in {
    val i = new AtomicInteger()
    def throww(): Any = throw new IOException(i.incrementAndGet().toString)
    try {
      Retry(throww(), limit = 10, sleepInMillis = 0, noExcluded: _*)
      assert(false)
    } catch {
      case ioe: IOException =>
        assert(ioe.getMessage == "1")
        assert(i.get() == 10)
    }
  }

  "retry" should "throw recover" in {
    for (recoveryStep <- (1 to 14)) {
      val i = new AtomicInteger()
      val value = Retry({
        val thisI = i.incrementAndGet()
        if (thisI == recoveryStep) "recover" else throw new IOException(thisI.toString)
      }, limit = 15, sleepInMillis = 0, noExcluded: _*)
      assert(value == "recover")
    }
  }
}
