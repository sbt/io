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

import java.io.{ ByteArrayOutputStream, InterruptedIOException, IOException, OutputStream }
import java.util.concurrent.{ CountDownLatch, Executors, TimeUnit }
import java.util.zip.{ ZipEntry, ZipException, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.ZipTestSupport.{ crcOf, firstDifference, hasSignature }
import scala.concurrent.ExecutionContext
import ZipConstants.EndSig

/**
 * What a refusal or a broken stream leaves behind, and whether a caller that swallows one and carries
 * on is held to the same thing the reference holds it to — an entry left open, a size already
 * counted, a deflater already finished, a sink that throws.
 */
class ParallelZipFailureSpec extends AnyFunSuite with ParallelZipSupport {

  /**
   * Fails every write and records that it was closed. `close` fails too with its own message, unless
   * told to close cleanly so that nothing masks what the writer's own `close` throws.
   */
  private final class DeadStream(failOnClose: Boolean = true) extends OutputStream {
    var closed = false
    override def write(b: Int): Unit = throw new IOException("No space left on device")
    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      throw new IOException("No space left on device")
    override def close(): Unit = {
      closed = true
      if (failOnClose) throw new IOException("secondary, while closing")
    }
  }

  /**
   * Queues one entry, then opens an oversized one so that `startStreaming` drains into the dead
   * stream. That leaves the writer mid-entry with no buffer, which is the state `close` has to
   * survive.
   */
  private def failMidEntry(out: OutputStream): (ParallelZipOutputStream, Throwable) = {
    val w = new ParallelZipOutputStream(out)
    val body = new Array[Byte](200000) // past the writer's 64KB sink buffer, so the write lands
    new java.util.Random(9).nextBytes(body)
    // Which call fails depends on where the entry is deflated: at `closeEntry` when that happens on
    // this thread, and at the next `putNextEntry` when it was queued and this is the drain.
    val thrown = intercept[IOException] {
      w.putNextEntry(entry("first.bin", body.length))
      w.write(body, 0, body.length)
      w.closeEntry()
      val oversized = new ZipEntry("big.bin")
      oversized.setTime(stamp)
      oversized.setSize(ParallelZipOutputStream.MaxEntryBytes + 1)
      w.putNextEntry(oversized)
    }
    (w, thrown)
  }

  test("a wrong declared size leaves behind the bytes the reference leaves behind") {
    // the reference writes the header and the content before it is in any position to check them, so a
    // refusal part way through leaves a particular prefix. `IO` discards it; a direct caller can see it.
    val body = ("class A { def f = 1 } " * 100).getBytes("UTF-8")
    val (compressed, crc) = deflatedSizes(body)
    def written(make: ByteArrayOutputStream => ZipSink): Array[Byte] = {
      val out = new ByteArrayOutputStream
      val w = make(out)
      val e = new ZipEntry("a.class")
      e.setTime(stamp)
      e.setSize(body.length.toLong)
      e.setCompressedSize(
        compressed + 1
      ) // one byte out, so the header goes down and is then refused
      e.setCrc(crc)
      intercept[ZipException] {
        w.putNextEntry(e)
        w.write(body, 0, body.length)
        w.closeEntry()
        w.finish()
      }
      // ours holds bytes in its own buffer: closing gets them out without adding any, since the remembered
      // refusal stops the second finish before it writes
      try w.close()
      catch { case _: Throwable => () }
      out.toByteArray
    }
    val ours = written(new ParallelZipOutputStream(_))
    val reference = written(new ZipOutputStream(_))
    assert(
      java.util.Arrays.equals(ours, reference),
      s"diverged at ${firstDifference(ours, reference)} (${ours.length} against ${reference.length})"
    )
    assert(
      !hasSignature(ours, EndSig.toInt),
      "an end record over a refused entry, where the reference leaves the entry open and writes none"
    )
  }

  test("an entry the writer refused leaves an archive that cannot be completed") {
    // the same refusal reached from `closeEntry` rather than `finish`, where nothing else would have stopped
    // `close` from writing an end record over it. Which call it surfaces from depends on when the entry is
    // appended: inline as it is closed, or on a drain the next entry forces — both are covered here, and one
    // entry in flight at a time makes the second happen at the next entry rather than at finish.
    val body = ("class A " * 100).getBytes("UTF-8")
    val (compressed, crc) = deflatedSizes(body)
    val out = new ByteArrayOutputStream
    val w = holdingUpTo(out, ParallelZipOutputStream.MaxEntryBytes, window = 1L, parallelism = 1)
    val bad = new ZipEntry("bad.class")
    bad.setTime(stamp)
    bad.setSize(body.length.toLong)
    bad.setCompressedSize(compressed + 1)
    bad.setCrc(crc)
    val refused = intercept[ZipException] {
      w.putNextEntry(bad)
      w.write(body, 0, body.length)
      w.closeEntry()
      writeOne(w, "next.txt")
      w.finish()
    }.getMessage
    assert(refused.startsWith("invalid entry compressed size"), refused)
    // and the archive cannot be completed afterwards, however it is closed
    intercept[ZipException](w.finish())
    try w.close()
    catch { case _: ZipException => () }
    assert(
      !hasSignature(out.toByteArray, EndSig.toInt),
      "the archive was completed after an entry the writer had already refused"
    )
  }

  test("a wait an interrupt cuts short is refused as an IOException, with the flag put back") {
    // `CompletableFuture.get` answers an interrupt with a checked exception no caller of an
    // `OutputStream` is in any position to catch, and clears the flag on its way out. sbt cancels a
    // task by interrupting it, so an archive that let both through would lose the cancellation.
    // The flag is set before the wait rather than during it, so nothing here depends on timing
    val gate = new CountDownLatch(1)
    val threads = Executors.newFixedThreadPool(
      1,
      (r: Runnable) => {
        val t = new Thread(r, "parallel-zip-interrupt-spec")
        t.setDaemon(true)
        t
      }
    )
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(threads)
    val w = new ParallelZipOutputStream(new ByteArrayOutputStream, parallelism = 1)
    try {
      threads.execute(() => gate.await()) // occupies the pool, so the entry stays in flight
      writeOne(w, "a.txt")
      Thread.currentThread().interrupt()
      val refused = intercept[InterruptedIOException](w.finish())
      assert(Thread.interrupted(), "the interrupt flag was not put back")
      assert(refused.getCause.isInstanceOf[InterruptedException], s"cause was ${refused.getCause}")
    } finally {
      gate.countDown() // so the entry can finish deflating and the writer can close
      closeQuietly(w)
      val _ = threads.shutdownNow()
    }
  }

  test("a refusal names the entry responsible") {
    // one bad entry among thousands and nothing in the output says which. Only the repeated name is left to
    // answer that: every other refusal is now the reference's own message, in its wording, naming nothing.
    val w = parallel(new ByteArrayOutputStream)
    try {
      writeOne(w, "pkg/dup.class")
      val message = intercept[ZipException](w.putNextEntry(entry("pkg/dup.class"))).getMessage
      assert(
        message.contains("pkg/dup.class"),
        s"the refusal should name the entry responsible: $message"
      )
    } finally w.close()
  }

  test("a writer left mid-entry by a failed write can still be finished and closed") {
    val (w, primary) = failMidEntry(new DeadStream(failOnClose = false))
    assert(primary.getMessage === "No space left on device")

    // assert on finish, not close: close's finally flushes the sink and that flush throws its own
    // IOException, which would hide an NPE from the null buffer the failed open left behind
    val onFinish = intercept[IOException](w.finish())
    assert(
      onFinish.getMessage === "No space left on device",
      s"finish reported ${onFinish.getClass.getName}: ${onFinish.getMessage}, not the write " +
        "failure - the null buffer from the failed open leaked through"
    )
    intercept[IOException](w.close())
  }

  test("close releases the underlying stream even when finish throws") {
    val dead = new DeadStream
    val (w, _) = failMidEntry(dead)
    intercept[IOException](w.close())
    assert(dead.closed, "close must release the underlying stream even when finish throws")
  }

  test("a finish that failed is not reported as a whole archive when it is called again") {
    // the reference marks itself finished only once the directory is down, so a caller that catches a failure
    // and tries again fails again rather than being told the archive is complete
    val (w, _) = failMidEntry(new DeadStream(failOnClose = false))
    val first = intercept[IOException](w.finish()).getMessage
    val second = intercept[IOException](w.finish()).getMessage
    assert(second === first, s"the second finish reported $second, where the first reported $first")
    intercept[IOException](w.close())
    ()
  }

  test(
    "an entry refused on the drain a streamed entry forces cannot be followed by a whole archive"
  ) {
    // `startStreaming` drains everything queued before its own header goes down, and a refusal surfacing from
    // there is the same dead archive as one from `closeEntry` or `finish`. On a pool that cannot run anything
    // in parallel the entry is appended as it is closed instead and the refusal comes from there, so the whole
    // sequence is intercepted rather than any one call.
    val body = ("class A " * 100).getBytes("UTF-8")
    val (compressed, crc) = deflatedSizes(body)
    val out = new ByteArrayOutputStream
    val w = new ParallelZipOutputStream(out)
    val bad = new ZipEntry("bad.class")
    bad.setTime(stamp)
    bad.setSize(body.length.toLong)
    bad.setCompressedSize(compressed + 1)
    bad.setCrc(crc)
    val refused = intercept[ZipException] {
      w.putNextEntry(bad)
      w.write(body, 0, body.length)
      w.closeEntry()
      w.putNextEntry(directoryEntry("dir/")) // stored, so it streams, so it drains first
      w.closeEntry()
      w.finish()
    }.getMessage
    assert(refused.startsWith("invalid entry compressed size"), refused)
    try w.close()
    catch { case _: ZipException => () }
    assert(
      !hasSignature(out.toByteArray, EndSig.toInt),
      "the archive was completed after an entry refused on a drain"
    )
  }

  test("flush over a refused entry does what it does there, and finish still refuses") {
    // the reference's flush reaches the underlying stream and nothing else — it is not where a
    // refused entry surfaces. The entry stays open instead, so finish is still refused
    def outcome(make: ByteArrayOutputStream => ZipSink): (String, String) = {
      val body = ("class A " * 100).getBytes("UTF-8")
      val (compressed, crc) = deflatedSizes(body)
      val w = make(new ByteArrayOutputStream)
      val bad = new ZipEntry("bad.class")
      bad.setTime(stamp)
      bad.setSize(body.length.toLong)
      bad.setCompressedSize(compressed + 1) // one byte out, so closeEntry refuses it
      bad.setCrc(crc)
      intercept[ZipException] {
        w.putNextEntry(bad)
        w.write(body, 0, body.length)
        w.closeEntry()
        w.finish()
      }
      def step(run: => Unit): String =
        try { run; "ok" }
        catch { case refused: ZipException => refused.getMessage }
      val onFlush = step(w.flush())
      val onFinish = step(w.finish())
      try w.close()
      catch { case _: ZipException => () }
      (onFlush, onFinish)
    }
    val ours = outcome(new ParallelZipOutputStream(_))
    val reference = outcome(new ZipOutputStream(_))
    assert(ours === reference, s"ours $ours against the reference's $reference")
    assert(ours._2.startsWith("invalid entry compressed size"), ours._2)
  }

  test("an entry the reference refused stays open, as it does there") {
    // a caller that swallows the refusal and carries on is refused again, and against what it has
    // written since — not against a remembered message about the entry it left behind
    def trace(make: ByteArrayOutputStream => ZipSink): List[String] = {
      val w = make(new ByteArrayOutputStream)
      val body = "hello".getBytes("UTF-8")
      val sum = crcOf(body)
      // named apart from `declaring`, which declares sizes that are right rather than overstated
      def overstating(name: String): ZipEntry = {
        val e = new ZipEntry(name)
        e.setTime(stamp)
        e.setMethod(ZipEntry.STORED)
        e.setSize(99) // far more than is written, so closeEntry refuses it
        e.setCrc(sum)
        e
      }
      def step(what: String)(run: => Unit): String =
        try { run; s"$what: ok" }
        catch { case refused: ZipException => s"$what: ${refused.getMessage}" }
      List(
        step("put bad")(w.putNextEntry(overstating("bad.bin"))),
        step("write")(w.write(body, 0, body.length)),
        step("close bad")(w.closeEntry()),
        step("put good")(w.putNextEntry(overstating("good.bin"))),
        step("write")(w.write(body, 0, body.length)),
        step("close good")(w.closeEntry()),
        step("finish")(w.finish())
      )
    }
    val ours = trace(new ParallelZipOutputStream(_))
    val reference = trace(new ZipOutputStream(_))
    assert(ours === reference, s"diverged:\n  ours      $ours\n  reference $reference")
  }

  test("a write past a refused deflated entry is refused, rather than spinning on the deflater") {
    // that entry keeps the deflater it finished, and a finished deflater takes no more input:
    // `deflate` hands back nothing while `needsInput` stays false, so a loop over it never ends.
    // Driven on a thread of its own, since a regression here hangs the suite rather than failing it
    def trace(make: ByteArrayOutputStream => ZipSink): List[String] = {
      val w = make(new ByteArrayOutputStream)
      val body = "hello".getBytes("UTF-8")
      val sum = crcOf(body)
      // named apart from `declaring`, which declares sizes that are right rather than overstated
      def overstating(name: String): ZipEntry = {
        val e = new ZipEntry(name)
        e.setTime(stamp)
        e.setMethod(ZipEntry.DEFLATED)
        e.setSize(
          99
        ) // more than is written, so closeEntry refuses it; all three so that it streams
        e.setCompressedSize(99)
        e.setCrc(sum)
        e
      }
      def step(what: String)(run: => Unit): String =
        try { run; s"$what: ok" }
        catch { case e: IOException => s"$what: ${e.getClass.getName}: ${e.getMessage}" }
      List(
        step("put")(w.putNextEntry(overstating("bad.bin"))),
        step("write")(w.write(body, 0, body.length)),
        step("close")(w.closeEntry()),
        // the length is looked at first there, so a zero-length write past the refusal is still fine
        step("write nothing")(w.write(body, 0, 0)),
        step("write again")(w.write(body, 0, body.length)),
        step("close again")(w.closeEntry())
      )
    }
    def within30Seconds(run: => List[String]): List[String] = {
      var traced = List.empty[String]
      val thread = new Thread(() => traced = run)
      thread.setDaemon(true)
      thread.start()
      thread.join(TimeUnit.SECONDS.toMillis(30))
      assert(!thread.isAlive, "the writer never returned from a write past a refused entry")
      traced
    }
    val ours = within30Seconds(trace(new ParallelZipOutputStream(_)))
    val reference = within30Seconds(trace(new ZipOutputStream(_)))
    assert(ours === reference, s"diverged:\n  ours      $ours\n  reference $reference")
  }
}
