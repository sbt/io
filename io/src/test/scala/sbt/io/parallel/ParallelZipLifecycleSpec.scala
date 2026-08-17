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

package sbt.io.parallel

import java.io.{ ByteArrayOutputStream, IOException, OutputStream }
import java.util.zip.{ ZipException, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite

/**
 * The writer's own contract, driven the way a caller may get it wrong: opening an entry before
 * closing the last, finishing twice, writing with nothing open, flushing mid-archive, and the
 * arguments both writers refuse outright.
 */
class ParallelZipLifecycleSpec extends AnyFunSuite with ParallelZipSupport {
  test("finish is idempotent, so finish then close does not append a second directory") {
    bothWriters { (label, make) =>
      val out = new ByteArrayOutputStream
      val w = make(out)
      writeOne(w, "a.txt")
      w.finish()
      val afterFinish = out.size
      w.finish()
      assert(out.size === afterFinish, s"$label: a second finish appended more bytes")
      w.close()
      assert(out.size === afterFinish, s"$label: close after finish appended more bytes")
      assert(namesIn(out.toByteArray) === List("a.txt"), s"$label: archive no longer reads back")
    }
  }

  test("the parallel writer refuses an entry after finish, where the reference does not") {
    // measured: ZipOutputStream guards a repeated finish but not an entry after one, and appends a
    // header and content past the end record — bytes no reader references and strict tools reject
    val out = new ByteArrayOutputStream
    val reference = sequential(out)
    writeOne(reference, "a.txt")
    reference.finish()
    val afterFinish = out.size
    writeOne(reference, "late.txt")
    assert(out.size > afterFinish, "the reference is expected to append past its own end record")
    assert(namesIn(out.toByteArray) === List("a.txt"), "and no reader to see the late entry")

    val ours = new ByteArrayOutputStream
    val w = parallel(ours)
    writeOne(w, "a.txt")
    w.finish()
    val thrown = intercept[ZipException](w.putNextEntry(entry("late.txt")))
    assert(thrown.getMessage === "the archive is finished")
    assert(namesIn(ours.toByteArray) === List("a.txt"), "a late entry reached the archive")
  }

  test("both writers refuse an entry after close, in their own words") {
    val w = parallel(new ByteArrayOutputStream)
    writeOne(w, "a.txt")
    w.close()
    assert(
      intercept[ZipException](w.putNextEntry(entry("late.txt"))).getMessage
        === "the archive is finished"
    )

    // the reference refuses it too, as a closed stream rather than a finished archive
    val reference = sequential(new ByteArrayOutputStream)
    writeOne(reference, "a.txt")
    reference.close()
    val onReference = intercept[IOException](reference.putNextEntry(entry("late.txt")))
    assert(onReference.getMessage === "Stream closed")
  }

  test("a zero-length write is a no-op with no entry open, as it is in the reference") {
    bothWriters { (label, make) =>
      val out = new ByteArrayOutputStream
      val w = make(out)
      w.write(Array.emptyByteArray, 0, 0) // before any entry: the reference returns early
      writeOne(w, "a.txt")
      w.finish()
      w.close()
      assert(namesIn(out.toByteArray) === List("a.txt"), s"$label: archive no longer reads back")
    }
  }

  test("a write out of bounds is refused before the length is looked at, as in the reference") {
    bothWriters { (label, make) =>
      // (3, 0) and (-1, 0) are the ordering: a zero-length write returns early, but only once the
      // reference has found the bounds wrong
      Seq((0, 3), (-1, 1), (1, -1), (-1, 0), (3, 0)).foreach { case (off, len) =>
        val w = make(new ByteArrayOutputStream)
        w.putNextEntry(entry("a.txt", 3))
        withClue(s"$label, off $off len $len: ") {
          intercept[IndexOutOfBoundsException](w.write(new Array[Byte](2), off, len))
        }
      }
    }
  }

  test("a write with no entry open is refused in the reference's words") {
    def refusal(make: ByteArrayOutputStream => ZipSink): String =
      refusalFrom(make)(_.write(Array[Byte](1), 0, 1))
    assert(refusal(parallelZip(_)) === refusal(new ZipOutputStream(_)))
  }

  test("a parallelism past anything the window allows in flight is still constructible") {
    // a caller passes this through from a build setting, and it bounds what the writer recycles
    Seq(1, 1000000, Int.MaxValue).foreach { parallelism =>
      val out = new ByteArrayOutputStream
      val w = new ParallelZipOutputStream(out, parallelism = parallelism)
      try {
        writeOne(w, "a.txt")
        w.finish()
      } finally w.close()
      assert(namesIn(out.toByteArray) === List("a.txt"), s"at parallelism $parallelism")
    }
  }

  test("a parallelism below 1 is refused") {
    Seq(0, -1, Int.MinValue).foreach { p =>
      intercept[IllegalArgumentException](
        new ParallelZipOutputStream(new ByteArrayOutputStream, parallelism = p)
      )
    }
  }

  test("closeEntry with no entry open is a no-op") {
    bothWriters { (label, make) =>
      val out = new ByteArrayOutputStream
      val w = make(out)
      w.closeEntry() // no entry open
      writeOne(w, "a.txt")
      w.closeEntry() // already closed by writeOne
      w.closeEntry() // again, no entry open
      w.finish()
      w.close()
      assert(
        namesIn(out.toByteArray) === List("a.txt"),
        s"$label: archive corrupt after no-op closeEntry"
      )
    }
  }

  test("flush drains queued entries and matches the reference") {
    val body = ("class A { def f = 1 } " * 50).getBytes("UTF-8")
    sameAsReference("flush") { w =>
      w.putNextEntry(entry("a.txt", body.length))
      w.write(body, 0, body.length)
      w.closeEntry()
      w.flush()
      w.putNextEntry(entry("b.txt", body.length))
      w.write(body, 0, body.length)
      w.closeEntry()
    }
  }

  test("putNextEntry without closeEntry auto-closes the previous entry, byte for byte") {
    val body = ("class A { def f = 1 } " * 50).getBytes("UTF-8")
    sameAsReference("auto-close") { w =>
      w.putNextEntry(entry("a.txt", body.length))
      w.write(body, 0, body.length)
      w.putNextEntry(entry("b.txt", body.length))
      w.write(body, 0, body.length)
      w.closeEntry()
    }
  }
}
