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

import java.io.{ ByteArrayOutputStream, OutputStream }
import java.util.concurrent.{
  CompletableFuture,
  ExecutorService,
  Executors,
  ForkJoinPool,
  ForkJoinWorkerThread,
  RejectedExecutionException,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicInteger
import java.util.zip.{ ZipEntry, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.IO
import sbt.io.ZipTestSupport.sameBytes
import scala.concurrent.ExecutionContext

/**
 * The archive itself, byte for byte against the `ZipOutputStream` it stands in for: that routing an
 * entry — held, streamed, deflated where it was written, or handed to a context — never reaches the
 * bytes. What one entry's own record says is [[ParallelZipEntrySpec]]; misuse is
 * [[ParallelZipLifecycleSpec]] and [[ParallelZipFailureSpec]].
 */
class ParallelZipSpec extends AnyFunSuite with ParallelZipSupport {

  /** Counts what a context was given, and how much of it ran anywhere but the threads it owns. */
  private final class Recording(to: ExecutorService) extends ExecutionContext {
    val submitted = new AtomicInteger
    val elsewhere = new AtomicInteger
    def execute(r: Runnable): Unit = {
      val _ = submitted.incrementAndGet()
      to.execute { () =>
        if (Thread.currentThread.getName != Recording.ThreadName) {
          val _ = elsewhere.incrementAndGet()
        }
        r.run()
      }
    }
    def reportFailure(cause: Throwable): Unit = throw cause
  }

  private object Recording {
    final val ThreadName = "parallel-zip-spec-context"
    def pool(): ExecutorService =
      Executors.newFixedThreadPool(2, (r: Runnable) => new Thread(r, ThreadName))
  }

  /** Class-sized entries of varied length that deflate to about half of it, so deflate does real work. */
  private def corpus(count: Int, seed: Long): Seq[(String, Array[Byte], Long)] = {
    val rnd = new java.util.Random(seed)
    (1 to count).map { i =>
      val body = new Array[Byte](500 + rnd.nextInt(20000))
      rnd.nextBytes(body)
      var j = 0
      while (j < body.length) { body(j) = 0; j += 2 }
      (s"pkg/C$i.class", body, stamp)
    }
  }

  /** What `make`'s writer leaves after `IO`'s own entry walk over `dirs` and `files`. */
  private def through(
      make: ByteArrayOutputStream => ZipSink,
      dirs: Seq[String],
      files: Seq[(String, Array[Byte], Long)]
  ): Array[Byte] = {
    val out = new ByteArrayOutputStream
    writeThrough(make(out), dirs, files, stamp)
    out.toByteArray
  }

  test("ParallelZipOutputStream writes the same bytes as ZipOutputStream") {
    val dirs = Seq("pkg/")
    val files = Seq(
      ("pkg/a.class", ("class A " * 50).getBytes("UTF-8"), stamp),
      ("empty.txt", Array.emptyByteArray, stamp),
      ("tiny.txt", "no".getBytes("UTF-8"), stamp),
      ("\u00e9\u4e2d.txt", "unicode".getBytes("UTF-8"), stamp)
    )
    sameBytes(
      "ParallelZipOutputStream",
      through(new ParallelZipOutputStream(_), dirs, files),
      through(new ZipOutputStream(_), dirs, files),
      "ZipOutputStream"
    )
  }

  test("ParallelZipOutputStream writes an empty archive") {
    sameBytes(
      "an empty archive",
      through(new ParallelZipOutputStream(_), Nil, Nil),
      through(new ZipOutputStream(_), Nil, Nil),
      "ZipOutputStream"
    )
  }

  test("ParallelZipOutputStream writes identical bytes at any parallelism") {
    val files = corpus(300, seed = 4)
    val want = through(new ZipOutputStream(_), Seq("pkg/"), files)
    Seq(1, 2, 8, ParallelZipOutputStream.DefaultParallelism).foreach { p =>
      val ours = through(new ParallelZipOutputStream(_, parallelism = p), Seq("pkg/"), files)
      sameBytes(s"parallelism $p", ours, want, "the reference")
    }
  }

  test("an entry deflated on the writing thread produces the same archive, byte for byte") {
    // `ForkJoinPool.getCommonPoolParallelism` is above 1 wherever these tests run, so overriding the
    // choice is the only thing that reaches the branch a single-core machine takes, where an entry is
    // deflated where it was written rather than handed to the pool
    class OnThisThread(out: ByteArrayOutputStream, hold: Long)
        extends ParallelZipOutputStream(out) {
      override protected def maxEntryBytes: Long = hold
      override protected def deflateOnThisThread: Boolean = true
    }
    val files = corpus(40, seed = 9)
    val want = through(new ZipOutputStream(_), Seq("pkg/"), files)
    // held and streamed both, since only the held path reaches the branch and only the archive
    // around it proves the two still interleave in order
    Seq(1024L, ParallelZipOutputStream.MaxEntryBytes).foreach { hold =>
      val ours = through(new OnThisThread(_, hold), Seq("pkg/"), files)
      sameBytes(s"hold $hold", ours, want, "the reference")
    }
  }

  test("entries deflate on a context the caller passed, byte for byte") {
    val threads = Recording.pool()
    implicit val ec: Recording = new Recording(threads)
    val files = corpus(40, seed = 21)
    try {
      sameBytes(
        "entries deflated on the caller's context",
        through(new ParallelZipOutputStream(_), Seq("pkg/"), files),
        through(new ZipOutputStream(_), Seq("pkg/"), files),
        "the reference"
      )
      // and the context was where the deflating happened, rather than a parameter nothing reads: one
      // handover per file entry, the directory being stored and so streamed, and none of it run
      // anywhere but the threads this context owns
      assert(ec.submitted.get === files.length)
      assert(ec.elsewhere.get === 0)
    } finally { val _ = threads.shutdownNow() }
  }

  test("an entry a context refuses is refused, rather than left in flight") {
    val stopped = Executors.newFixedThreadPool(1)
    val _ = stopped.shutdownNow()
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(stopped)
    val out = new ByteArrayOutputStream
    val w = new ParallelZipOutputStream(out)
    w.putNextEntry(entry("a.txt"))
    w.write("body".getBytes("UTF-8"), 0, 4)
    // the refusal reaches the caller rather than becoming a future that nothing will ever complete
    intercept[RejectedExecutionException](w.closeEntry())
    // and the archive still closes, since a refused entry never joined the queue it would be drained
    // from — it is missing from the archive rather than holding it open
    w.close()
    assert(namesIn(out.toByteArray) === Nil)
  }

  test("a context the caller supplied is handed to, whatever commonPool would have said") {
    // the choice is a fact about `commonPool` — below two cores it gains nothing and on 8 it may run
    // nothing at all — so it has no bearing on a context that came from somewhere else. Asserted on
    // the choice rather than through an archive because the machine this runs on has cores, so an
    // archive would take the same path either way and could not tell a fixed answer from a right one
    class Peek(out: OutputStream)(implicit ec: ExecutionContext)
        extends ParallelZipOutputStream(out) {
      def handsOver: Boolean = !deflateOnThisThread
    }
    val threads = Recording.pool()
    try {
      val mine: ExecutionContext = ExecutionContext.fromExecutor(threads)
      assert(new Peek(new ByteArrayOutputStream)(mine).handsOver)
      // and `commonPool` as a context is still the pool's own business
      assert(
        new Peek(new ByteArrayOutputStream)(
          ParallelZipOutputStream.commonPoolContext
        ).handsOver === (ForkJoinPool.getCommonPoolParallelism > 1)
      )
    } finally { val _ = threads.shutdownNow() }
  }

  test("the context IO offers is the pool the handover was already using") {
    // `CompletableFuture` sends work to `commonPool` wherever this writer hands an entry over, and
    // below two cores it would send it to a thread of its own — which is where the writer deflates
    // on its own thread instead, so `commonPool` is the whole of what `IO.Implicits.deflateContext` has to name
    assume(ForkJoinPool.getCommonPoolParallelism > 1)
    assert(IO.Implicits.deflateContext eq ParallelZipOutputStream.commonPoolContext)
    val ran = new CompletableFuture[ForkJoinPool]
    ParallelZipOutputStream.commonPoolContext.execute { () =>
      val _ = ran.complete(Thread.currentThread match {
        case worker: ForkJoinWorkerThread => worker.getPool
        case _                            => null
      })
    }
    assert(ran.get(30, TimeUnit.SECONDS) eq ForkJoinPool.commonPool())
  }

  test("an entry handed over in more writes than it is worth holding is streamed, byte for byte") {
    // where a write ended is four bytes to remember, which is nothing against a write of any size and
    // everything against a write of one byte: past a million of them the entry streams instead, and
    // has to come out as the reference wrote it, having taken every one of those writes as it came
    val body = new Array[Byte](1100000)
    new java.util.Random(31).nextBytes(body)
    var i = 0
    while (i < body.length) { body(i) = 0; i += 3 }
    def archive(make: ByteArrayOutputStream => ZipSink): Array[Byte] = {
      val out = new ByteArrayOutputStream
      val w = make(out)
      val e = new ZipEntry("drip.bin")
      e.setTime(stamp)
      w.putNextEntry(e)
      var at = 0
      while (at < body.length) {
        w.write(
          body(at) & 0xff
        ) // a byte at a time, which is what makes the list outweigh the bytes
        at += 1
      }
      w.closeEntry()
      w.finish()
      w.close()
      out.toByteArray
    }
    sameBytes(
      "an entry written a byte at a time",
      archive(new ParallelZipOutputStream(_)),
      archive(new ZipOutputStream(_)),
      "the reference"
    )
  }

  test("an entry handed over in one array past the hold threshold is streamed, byte for byte") {
    // the threshold is consulted before the copy, so an entry arriving whole is never held whole
    // first. What that bounds is memory and is measured against a constrained heap rather than here;
    // what this pins is that deciding it earlier left the bytes alone, since the deflater now sees
    // the entry as two calls where it saw one
    val body = new Array[Byte](256 * 1024)
    new java.util.Random(12).nextBytes(body)
    var j = 0
    while (j < body.length) { body(j) = 0; j += 3 }
    def drive(w: ZipSink): Unit = {
      val e = new ZipEntry("big.bin")
      e.setTime(stamp)
      w.putNextEntry(e) // no declared size, so only the write itself can cross the threshold
      w.write(body, 0, body.length) // and it arrives in one call
      w.closeEntry()
    }
    Seq(1024L, 64L * 1024, body.length - 1L, body.length.toLong).foreach { hold =>
      sameAsReference(s"hold $hold", holdingUpTo(_, hold))(drive)
    }
  }

  test("ParallelZipOutputStream streams an entry past the size limit") {
    val big = ("big " * 40000).getBytes("UTF-8") // ~160KB
    val files = Seq(
      ("big.txt", big, stamp),
      ("a.txt", ("a" * 500).getBytes("UTF-8"), stamp)
    )
    sameBytes(
      "an entry past the size limit",
      through(
        holdingUpTo(_, 1024L, window = 1024L),
        Seq("pkg/"),
        files
      ),
      through(new ZipOutputStream(_), Seq("pkg/"), files),
      "the reference"
    )
  }

  test("ParallelZipOutputStream buffers an entry whose size was not declared") {
    // With no declared size the buffer grows past what it holds, and it is lent rather than copied, so
    // the crc, the deflater's input and the compressed size must all take the length, not the array.
    val body = ("mixed content " * 3000).getBytes("UTF-8") // 42000 bytes, so the buffer overshoots
    def writeUndeclared(w: ZipSink): Unit = {
      val e = new ZipEntry("a.txt")
      e.setTime(stamp)
      w.putNextEntry(e)
      var off = 0
      while (off < body.length) {
        val n = math.min(777, body.length - off)
        w.write(body, off, n)
        off += n
      }
      w.closeEntry()
      w.finish()
      w.close()
    }
    val got = new ByteArrayOutputStream
    writeUndeclared(new ParallelZipOutputStream(got))
    val want = new ByteArrayOutputStream
    writeUndeclared(new ZipOutputStream(want))
    sameBytes("an undeclared size", got.toByteArray, want.toByteArray, "the reference")
  }

  test("ParallelZipOutputStream streams an entry whose size was not declared") {
    val big = ("big " * 40000).getBytes("UTF-8")
    val got = new ByteArrayOutputStream
    val w = holdingUpTo(got, 1024L, window = 1024L)
    val e = new ZipEntry("big.txt")
    e.setTime(stamp)
    // no setSize: the writer only learns the size as bytes arrive, in 8KB pieces
    w.putNextEntry(e)
    var off = 0
    while (off < big.length) {
      val n = math.min(8192, big.length - off)
      w.write(big, off, n)
      off += n
    }
    w.closeEntry()
    w.finish()
    w.close()

    sameBytes(
      "a streamed entry of undeclared size",
      got.toByteArray,
      through(new ZipOutputStream(_), Nil, Seq(("big.txt", big, stamp))),
      "the reference"
    )
  }

  test("a corpus mixing one multi-megabyte entry among small ones matches the reference") {
    // an entry this size is held rather than streamed, and the buffer it deflates into is many times
    // a block — so the free list, the window's accounting and the block walk are all handed sizes
    // the single-size fixtures never reach. None of that is meant to touch the bytes, which is what
    // this pins: the size band between a class file and the streaming threshold, written out whole
    val big =
      ("big " * 1500000).getBytes("UTF-8") // 6MB, held rather than streamed, and quick to deflate
    val small = corpus(80, seed = 41)
    val files = small.take(40) ++ Seq(("big.bin", big, stamp)) ++ small.drop(40)
    sameBytes(
      "a mixed corpus",
      through(new ParallelZipOutputStream(_), Seq("pkg/"), files),
      through(new ZipOutputStream(_), Seq("pkg/"), files),
      "the reference"
    )
  }

  test("ParallelZipOutputStream holds to its byte budget while staying correct") {
    val rnd = new java.util.Random(5)
    val files = (1 to 40).map { i =>
      val body = new Array[Byte](100000)
      rnd.nextBytes(body)
      (s"e$i.bin", body, stamp)
    }
    // 4MB of input through a 64KB window: every entry forces a drain
    sameBytes(
      "a 64KB window",
      through(
        holdingUpTo(_, ParallelZipOutputStream.MaxEntryBytes, window = 64L * 1024, parallelism = 4),
        Nil,
        files
      ),
      through(new ZipOutputStream(_), Nil, files),
      "the reference"
    )
  }

  test("ParallelZipOutputStream interleaves pooled and streamed entries correctly") {
    val small = ("class A " * 50).getBytes("UTF-8")
    val big = ("big " * 40000).getBytes("UTF-8") // ~160KB, past the 1024-byte threshold below
    val files = Seq(
      ("a.class", small, stamp),
      ("b.class", small, stamp),
      ("big1.txt", big, stamp),
      ("c.class", small, stamp),
      ("big2.txt", big, stamp),
      ("d.class", small, stamp),
      ("e.class", small, stamp)
    )
    sameBytes(
      "pooled and streamed entries interleaved",
      through(
        holdingUpTo(_, 1024L, window = 1024L),
        Seq("pkg/"),
        files
      ),
      through(new ZipOutputStream(_), Seq("pkg/"), files),
      "the reference"
    )
  }

  test("DefaultParallelism follows the processor count") {
    assert(ParallelZipOutputStream.DefaultParallelism >= 1)
    assert(
      ParallelZipOutputStream.DefaultParallelism ===
        math.max(1, Runtime.getRuntime.availableProcessors)
    )
  }
}
