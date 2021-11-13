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
