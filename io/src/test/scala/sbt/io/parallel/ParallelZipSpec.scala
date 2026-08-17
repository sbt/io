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
  CountDownLatch,
  ExecutorService,
  Executors,
  ForkJoinPool,
  ForkJoinWorkerThread,
  FutureTask,
  RejectedExecutionException,
  TimeUnit,
  TimeoutException
}
import java.util.concurrent.atomic.AtomicInteger
import java.util.zip.{ ZipEntry, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.IO
import sbt.io.ZipTestSupport.sameBytes
import scala.concurrent.ExecutionContext

/**
 * The archive itself, byte for byte against the `ZipOutputStream` it stands in for: that how an
 * entry was routed never reaches the bytes. One entry's own record is [[ParallelZipEntrySpec]].
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

  private def running(how: Runnable => Unit): ExecutionContext = new ExecutionContext {
    def execute(runnable: Runnable): Unit = how(runnable)
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
      body.indices.by(2).foreach(body(_) = 0)
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
      through(parallelZip(_), dirs, files),
      through(new ZipOutputStream(_), dirs, files),
      "ZipOutputStream"
    )
  }

  test("ParallelZipOutputStream writes an empty archive") {
    sameBytes(
      "an empty archive",
      through(parallelZip(_), Nil, Nil),
      through(new ZipOutputStream(_), Nil, Nil),
      "ZipOutputStream"
    )
  }

  test("ParallelZipOutputStream writes identical bytes at any parallelism") {
    val files = corpus(300, seed = 4)
    val want = through(new ZipOutputStream(_), Seq("pkg/"), files)
    Seq(1, 2, 8, IO.defaultParallelism).foreach { p =>
      val ours = through(new ParallelZipOutputStream(_, parallelism = p), Seq("pkg/"), files)
      sameBytes(s"parallelism $p", ours, want, "the reference")
    }
  }

  test("an entry deflated on the writing thread produces the same archive, byte for byte") {
    // a context that runs what it is given right where it was given it, which is what a single-core
    // machine amounts to: the deflating lands on the writing thread rather than on a pool
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor((r: Runnable) => r.run())
    val files = corpus(40, seed = 9)
    val want = through(new ZipOutputStream(_), Seq("pkg/"), files)
    // held and streamed both, since only the held path hands anything over and only the archive
    // around it proves the two still interleave in order
    Seq(1024L, ParallelZipOutputStream.MaxEntryBytes).foreach { hold =>
      val ours = through(holdingUpTo(_, hold), Seq("pkg/"), files)
      sameBytes(s"hold $hold", ours, want, "the reference")
    }
  }

  test("a context that never runs what it is given is deflated past rather than waited on") {
    // the writing thread claims a task the context has not started, so a context that accepts work
    // and drops it — a common pool at no parallelism is the one in the wild — cannot wedge the writer
    val dropped = new AtomicInteger
    implicit val ec: ExecutionContext = new ExecutionContext {
      def execute(r: Runnable): Unit = { val _ = dropped.incrementAndGet() }
      def reportFailure(cause: Throwable): Unit = throw cause
    }
    val files = corpus(20, seed = 34)
    sameBytes(
      "an archive a context contributed nothing to",
      through(parallelZip(_), Seq("pkg/"), files),
      through(new ZipOutputStream(_), Seq("pkg/"), files),
      "the reference"
    )
    assert(dropped.get === files.length)
  }

  test("the writer finishes whatever a context does with the work it is given") {
    // a wedged writer hangs the suite rather than failing it, so each context is driven on a thread
    // of its own against a deadline. None of these ever completes a task the writer does not claim
    val files = corpus(30, seed = 77)
    val want = through(new ZipOutputStream(_), Seq("pkg/"), files)
    val occupied = Executors.newFixedThreadPool(1)
    val busy = new CountDownLatch(1)
    occupied.execute(() => busy.await())
    val cases = Seq[(String, ExecutionContext)](
      "a context that runs nothing" -> running(_ => ()),
      "a context that runs it where it was given" -> running(_.run()),
      "a context whose only thread is occupied" -> ExecutionContext.fromExecutor(occupied)
    )
    try
      cases.foreach { case (what, context) =>
        val archived = new FutureTask[Array[Byte]](() => {
          implicit val ec: ExecutionContext = context
          through(parallelZip(_), Seq("pkg/"), files)
        })
        val driver = new Thread(archived, "parallel-zip-deadlock-spec")
        driver.setDaemon(true)
        driver.start()
        val ours =
          try archived.get(30, TimeUnit.SECONDS)
          catch { case _: TimeoutException => fail(s"$what left the writer waiting") }
        sameBytes(what, ours, want, "the reference")
      }
    finally {
      busy.countDown()
      val _ = occupied.shutdownNow()
    }
  }

  test("entries deflate on a context the caller passed, byte for byte") {
    val threads = Recording.pool()
    implicit val ec: Recording = new Recording(threads)
    val files = corpus(40, seed = 21)
    try {
      sameBytes(
        "entries deflated on the caller's context",
        through(parallelZip(_), Seq("pkg/"), files),
        through(new ZipOutputStream(_), Seq("pkg/"), files),
        "the reference"
      )
      // and every entry was offered to the context rather than to a parameter nothing reads: one
      // handover per file entry, the directory being stored and so streamed. Which thread deflated
      // is not asserted — the writing thread claims whatever the context has not started yet
      assert(ec.submitted.get === files.length)
      assert(ec.elsewhere.get === 0)
    } finally { val _ = threads.shutdownNow() }
  }

  test("an entry a context refuses is refused, rather than left in flight") {
    val stopped = Executors.newFixedThreadPool(1)
    val _ = stopped.shutdownNow()
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(stopped)
    val out = new ByteArrayOutputStream
    val w = parallelZip(out)
    w.putNextEntry(entry("a.txt"))
    w.write("body".getBytes("UTF-8"), 0, 4)
    // the refusal reaches the caller rather than becoming a future that nothing will ever complete
    intercept[RejectedExecutionException](w.closeEntry())
    // the entry it never queued is the entry `close` never sweeps, so what it held — the block it
    // was written into and the buffer it would have deflated to — comes back here or nowhere
    assert(w.recycledBuffers === 2, "a refused entry dropped its buffers instead of recycling them")
    // and the archive still closes, since a refused entry never joined the queue it would be drained
    // from — it is missing from the archive rather than holding it open
    w.close()
    assert(namesIn(out.toByteArray) === Nil)
  }

  test("the context IO offers is commonPool") {
    assume(ForkJoinPool.getCommonPoolParallelism > 1)
    val ran = new CompletableFuture[ForkJoinPool]
    IO.Implicits.zipContext.execute { () =>
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
    body.indices.by(3).foreach(body(_) = 0)
    def archive(make: ByteArrayOutputStream => ZipSink): Array[Byte] = {
      val out = new ByteArrayOutputStream
      val w = make(out)
      val e = new ZipEntry("drip.bin")
      e.setTime(stamp)
      w.putNextEntry(e)
      // a byte at a time, which is what makes the list of write boundaries outweigh the bytes
      body.foreach(b => w.write(b & 0xff))
      w.closeEntry()
      w.finish()
      w.close()
      out.toByteArray
    }
    sameBytes(
      "an entry written a byte at a time",
      archive(parallelZip(_)),
      archive(new ZipOutputStream(_)),
      "the reference"
    )
  }

  test("an entry handed over in one array past the hold threshold is streamed, byte for byte") {
    // the threshold is consulted before the copy, so an entry arriving whole is never held whole
    // first. This pins that deciding it earlier left the bytes alone, the deflater now seeing the
    // entry as two calls where it saw one
    val body = new Array[Byte](256 * 1024)
    new java.util.Random(12).nextBytes(body)
    body.indices.by(3).foreach(body(_) = 0)
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
      writeInChunks(w, body, 777)
      w.closeEntry()
      w.finish()
      w.close()
    }
    val got = new ByteArrayOutputStream
    writeUndeclared(parallelZip(got))
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
    writeInChunks(w, big, 8192)
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
    // the size band between a class file and the streaming threshold: held, with a deflate buffer
    // many times a block, so the free list, the window accounting and the block walk are all handed
    // sizes the single-size fixtures never reach
    val big =
      ("big " * 1500000).getBytes("UTF-8") // 6MB, held rather than streamed, and quick to deflate
    val small = corpus(80, seed = 41)
    val files = small.take(40) ++ Seq(("big.bin", big, stamp)) ++ small.drop(40)
    sameBytes(
      "a mixed corpus",
      through(parallelZip(_), Seq("pkg/"), files),
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

  test("IO.defaultParallelism follows the processor count") {
    assert(IO.defaultParallelism >= 1)
    assert(IO.defaultParallelism === math.max(1, Runtime.getRuntime.availableProcessors))
  }
}
