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

package sbt.io

import java.io.{ BufferedOutputStream, File, FileOutputStream }
import java.lang.management.ManagementFactory
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import java.util.jar.Manifest
import org.openjdk.jmh.annotations._
import sbt.io.parallel.ParallelZipOutputStream
import scala.concurrent.ExecutionContext
// the arms that do not name a context take the one `IO` offers, which is what a caller gets by default
import IO.Implicits.zipContext

/**
 * Compares `IO.zipParallel` and `IO.jarParallel` against `IO.zip` and `IO.jar`, through `IO` rather
 * than the writers underneath so an arm measures what a caller gets.
 *
 * The `oneThread` arms isolate constant-factor cost from the parallelism win. They are not the same
 * stream — this writer buffers its sink in 64 KB against the reference's 512 — so slower than
 * sequential there is a signal and faster is not one. The `mixed` arms are the size band where the
 * in-flight window rather than `parallelism` decides when to drain.
 *
 * Allocation is checked in setup rather than left to `-prof gc`, and a bound past its ratio stops
 * the run: sbt still calls that a success, so an empty result table is what says it happened.
 *
 * Sized for a dev loop at about three minutes. For a number worth quoting raise them back —
 * `-wi 3 -i 5 -r 5` at least — and run it on a quiet machine.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class ZipBenchmark {
  import ZipBenchmark._

  private var temp: File = _
  private var many: Seq[(File, String)] = _
  private var oneBig: Seq[(File, String)] = _
  private var mixed: Seq[(File, String)] = _

  @Setup
  def setup(): Unit = {
    temp = Files.createTempDirectory("io-bench").toFile
    val rnd = new java.util.Random(1)

    val smallDir = new File(temp, "many")
    many = (1 to SmallEntryCount).map { i =>
      val name = s"pkg/sub${i % SubdirectoryCount}/C$i.class"
      val f = new File(smallDir, name)
      IO.createDirectory(f.getParentFile)
      val n = MinEntryBytes + rnd.nextInt(MaxEntryBytes - MinEntryBytes)
      val body = new Array[Byte](n)
      rnd.nextBytes(body)
      var j = 0
      while (j < n) { body(j) = 0; j += 2 } // half compressible, so deflate does real work
      IO.write(f, body)
      f -> name
    }

    val bigFile = new File(temp, "big.bin")
    val out = new BufferedOutputStream(new FileOutputStream(bigFile), BufferBytes)
    try {
      val chunk = new Array[Byte](BufferBytes)
      var written = 0L
      val size = BigEntryBytes
      while (written < size) {
        rnd.nextBytes(chunk)
        var i = 0
        while (i < chunk.length) { chunk(i) = 0; i += 2 }
        out.write(chunk)
        written += chunk.length
      }
    } finally out.close()
    require(
      BigEntryBytes > ParallelZipOutputStream.MaxEntryBytes,
      "the large entry has to be past the threshold, or it measures the held path twice"
    )
    oneBig = Seq(bigFile -> "big.bin")

    require(
      MidEntryBytes < ParallelZipOutputStream.MaxEntryBytes,
      "the mid-sized entries have to stay under the threshold, or they stream and leave no buffer"
    )
    val mixedDir = new File(temp, "mixed")
    mixed = many ++ (1 to MidEntryCount).map { i =>
      val name = s"lib/blob$i.bin"
      val f = new File(mixedDir, name)
      IO.createDirectory(f.getParentFile)
      val body = new Array[Byte](MidEntryBytes)
      rnd.nextBytes(body)
      var j = 0
      while (j < body.length) { body(j) = 0; j += 2 }
      IO.write(f, body)
      f -> name
    }

    println(s"many:   ${many.map(_._1.length()).sum / BytesPerMegabyte}MB in ${many.size} entries")
    println(s"oneBig: ${bigFile.length() / BytesPerMegabyte}MB in 1 entry")
    println(
      s"mixed:  ${mixed.map(_._1.length()).sum / BytesPerMegabyte}MB in ${mixed.size} entries"
    )
    checkAllocation()
  }

  @TearDown
  def tearDown(): Unit = IO.delete(temp)

  /**
   * Bytes allocated writing one archive each way, before anything is timed. Across every thread, the
   * pool warmed first: a thread that came and went between readings would take its count with it.
   */
  private def checkAllocation(): Unit = {
    val checks = Seq[(String, File => Unit)](
      ("zip, 4000 entries", IO.zip(many, _, FixedTime)),
      ("zipParallel, 4000 entries", IO.zipParallel(many, _, FixedTime)),
      ("zip, one large entry", IO.zip(oneBig, _, FixedTime)),
      ("zipParallel, one large entry", IO.zipParallel(oneBig, _, FixedTime))
    )

    val measured = checks.map { case (what, write) =>
      sized(write) // warm: the pool's threads, the probe's lazy vals, the JIT
      val before = allocatedEverywhere()
      sized(write)
      (what, allocatedEverywhere() - before)
    }
    measured.foreach { case (what, bytes) =>
      println(f"alloc: $what%-30s ${bytes / BytesPerMegabyte.toDouble}%8.1f MB/archive")
    }

    val byName = measured.toMap
    val manyRatio = byName("zipParallel, 4000 entries").toDouble / byName("zip, 4000 entries")
    require(
      manyRatio <= ManyEntriesAllocationRatio,
      f"the parallel writer allocated $manyRatio%.2f times what the sequential one did over 4000" +
        f" entries, past the $ManyEntriesAllocationRatio%.2f a recycled buffer per entry in flight" +
        " is worth — something stopped being recycled"
    )
    val held = ParallelZipOutputStream.MaxEntryBytes
    val bigRatio = byName("zipParallel, one large entry").toDouble / held
    require(
      bigRatio <= BigEntryAllocationRatio,
      f"the parallel writer allocated $bigRatio%.2f times what it holds writing one large entry," +
        f" past the $BigEntryAllocationRatio%.2f holding it in the blocks it arrived in costs"
    )
  }

  private def allocatedEverywhere(): Long = {
    val ids = threads.getAllThreadIds
    val each = threads.getThreadAllocatedBytes(ids)
    var total = 0L
    var i = 0
    while (i < each.length) {
      if (each(i) > 0) total += each(i)
      i += 1
    }
    total
  }

  /**
   * Returns the archive size, both to keep JMH from eliminating the work and because a size change is
   * the cheapest signal that compression was accidentally altered.
   */
  private def sized(write: File => Unit): Long = {
    val target = new File(temp, "out.tmp")
    write(target)
    target.length()
  }

  @Benchmark def manySmall_zip: Long = sized(IO.zip(many, _, FixedTime))

  @Benchmark def manySmall_zipParallel: Long = sized(IO.zipParallel(many, _, FixedTime))

  @Benchmark def manySmall_zipParallel_oneThread: Long =
    sized(IO.zipParallel(many, _, FixedTime)(onThisThread))

  @Benchmark def mixed_zip: Long = sized(IO.zip(mixed, _, FixedTime))

  @Benchmark def mixed_zipParallel: Long = sized(IO.zipParallel(mixed, _, FixedTime))

  @Benchmark def oneBig_zip: Long = sized(IO.zip(oneBig, _, FixedTime))

  @Benchmark def oneBig_zipParallel: Long = sized(IO.zipParallel(oneBig, _, FixedTime))

  @Benchmark def oneBig_zipParallel_oneThread: Long =
    sized(IO.zipParallel(oneBig, _, FixedTime)(onThisThread))

  @Benchmark def jar_jar: Long = sized(IO.jar(many, _, new Manifest, FixedTime))

  @Benchmark def jar_jarParallel: Long = sized(IO.jarParallel(many, _, new Manifest, FixedTime))

  @Benchmark def jar_jarParallel_oneThread: Long =
    sized(IO.jarParallel(many, _, new Manifest, FixedTime)(onThisThread))
}

object ZipBenchmark {

  /** 2010-01-01T00:00:00Z, so that neither writer is measured stamping the current time. */
  private final val FixedTime = Some(1262304000000L)

  /**
   * Deflates where the entry was written. Not a pool of one thread, which would still pipeline the
   * reading against the deflating — this leaves the writing thread doing both, which is the handover
   * machinery with none of the parallelism.
   */
  private val onThisThread: ExecutionContext = new ExecutionContext {
    def execute(runnable: Runnable): Unit = runnable.run()
    def reportFailure(cause: Throwable): Unit = throw cause
  }

  private val threads: com.sun.management.ThreadMXBean =
    ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]

  /**
   * What holding an entry to hand it over is allowed to cost over 4000 of them. Both arms are
   * dominated by reading the files in, so this is a ratio rather than a figure: measured at 1.22, and
   * a buffer that stopped being recycled would put it far past this rather than a little.
   */
  private final val ManyEntriesAllocationRatio = 1.5

  /**
   * For one entry past the threshold, as a multiple of what the writer holds rather than of the
   * sequential arm, which allocates almost nothing there. Measured at 1.01; doubling into one array
   * would be twice that.
   */
  private final val BigEntryAllocationRatio = 1.5

  /** The small-entry corpus: 4000 class-sized files spread over 40 packages. */
  private final val SmallEntryCount = 4000
  private final val SubdirectoryCount = 40
  private final val MinEntryBytes = 1024
  private final val MaxEntryBytes = 40 * 1024

  /**
   * What the `mixed` corpus adds to it: a few entries large enough that the buffer each deflates into
   * is many times a block, and still under the threshold past which an entry is streamed and leaves
   * no buffer behind at all. A shaded jar's blobs, which is where a real archive has them.
   */
  private final val MidEntryCount = 3
  private final val MidEntryBytes = 4 * 1024 * 1024

  /**
   * The large artifact an archive can be dominated by. A size of its own rather than one derived from
   * the writer's streaming threshold: the arm is here to say what a big entry costs, and one defined
   * as a little over the threshold would shrink whenever the threshold did and quietly measure less.
   */
  private final val BigEntryBytes = 72L << 20

  /** Building the large entry's file. */
  private final val BufferBytes = 64 * 1024

  private final val BytesPerMegabyte = 1024 * 1024
}
