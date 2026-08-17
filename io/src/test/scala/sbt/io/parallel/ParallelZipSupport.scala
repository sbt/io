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

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, OutputStream }
import java.util.zip.{ Deflater, ZipEntry, ZipException, ZipInputStream, ZipOutputStream }
import org.scalatest.Assertions
import sbt.io.IO
import sbt.io.ZipTestSupport.{ crcOf, firstDifference, sameBytes, EmptyCrc }
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

/**
 * What more than one parallel-zip suite drives the writers with. A fixture only one aspect needs
 * lives in that aspect's own suite instead.
 */
private[parallel] trait ParallelZipSupport extends Assertions {

  /**
   * Where these suites deflate: what a caller taking `IO`'s default gets. Named `ec` so that a test
   * naming its own context shadows this one rather than competing with it.
   */
  protected implicit val ec: ExecutionContext = IO.Implicits.zipContext

  // both writers under one name, so that a test drives the pair of them the same way. The parallel
  // one is a `ZipSink` already; the JDK's is adapted implicitly so that a test names the writer it
  // means — `new ZipOutputStream(out)` — rather than the adapter, which is not what is being tested
  protected implicit def sequentialWriter(z: ZipOutputStream): ZipSink = ZipSink(z)

  /**
   * The writers under test at the parallelism `IO` gives a caller that names none. The context is
   * taken here rather than closed over, so a test naming its own still shadows [[ec]].
   */
  protected def parallelZip(out: OutputStream)(implicit
      ec: ExecutionContext
  ): ParallelZipOutputStream =
    new ParallelZipOutputStream(out, IO.defaultParallelism)

  protected def parallelJar(out: OutputStream)(implicit
      ec: ExecutionContext
  ): ParallelJarOutputStream =
    new ParallelJarOutputStream(out, IO.defaultParallelism)

  /**
   * A writer that streams anything past `hold`, so that a test can drive the streaming path without
   * building an archive large enough to reach it. The bytes are the same either way — which is most
   * of what these suites are here to check — so only the path differs.
   */
  protected def holdingUpTo(
      out: OutputStream,
      hold: Long,
      window: Long = ParallelZipOutputStream.WindowBytes,
      parallelism: Int = IO.defaultParallelism
  )(implicit ec: ExecutionContext): ParallelZipOutputStream =
    new ParallelZipOutputStream(out, parallelism) {
      override protected def maxEntryBytes: Long = hold
      override protected def windowBytes: Long = window
    }

  /** 2010-01-01T00:00:00Z, shifted to the local zone the way `IO.archive` shifts every timestamp. */
  protected val stamp: Long =
    1262304000000L - java.util.TimeZone.getDefault.getOffset(1262304000000L)

  protected def entry(name: String, size: Int = 1, time: Long = stamp): ZipEntry = {
    val e = new ZipEntry(name)
    e.setTime(time)
    e.setSize(size.toLong)
    e
  }

  /** An empty directory entry, the only stored entry either writer accepts. */
  protected def directoryEntry(name: String, time: Long = stamp): ZipEntry = {
    val e = new ZipEntry(name)
    e.setTime(time)
    e.setSize(0)
    e.setMethod(ZipEntry.STORED)
    e.setCrc(EmptyCrc)
    e
  }

  /** The body an entry carries where its content is beside the point. */
  protected val oneByte: Array[Byte] = "x".getBytes("UTF-8")

  protected def writeOne(w: ZipSink, name: String): Unit = {
    w.putNextEntry(entry(name))
    w.write(oneByte, 0, oneByte.length)
    w.closeEntry()
  }

  /**
   * `body` handed to the open entry in `chunk`-sized writes, the last one whatever is left of it,
   * since where a write ends reaches the bytes.
   */
  protected def writeInChunks(w: ZipSink, body: Array[Byte], chunk: Int): Unit =
    body.indices.by(chunk).foreach(off => w.write(body, off, math.min(chunk, body.length - off)))

  /** One archive: what `make`'s writer leaves in its sink once `drive` has written to it. */
  protected def archive(make: ByteArrayOutputStream => ZipSink)(
      drive: ZipSink => Unit
  ): Array[Byte] = {
    val out = new ByteArrayOutputStream
    val w = make(out)
    drive(w)
    w.finish()
    w.close()
    out.toByteArray
  }

  /**
   * The same driving through this writer and the reference, which have to agree byte for byte. The
   * two writers are parameters rather than fixed so that a test can vary either — a threshold that
   * routes the entry the other way, or the jar pair in place of the zip one.
   */
  protected def sameAsReference(
      what: String,
      ours: ByteArrayOutputStream => ZipSink = parallelZip(_),
      reference: ByteArrayOutputStream => ZipSink = new ZipOutputStream(_)
  )(drive: ZipSink => Unit): Unit =
    sameBytes(what, archive(ours)(drive), archive(reference)(drive), "the reference")

  /**
   * The same driving through both writers where either may refuse it: they have to agree on the
   * archive or agree on the refusal, and one writing what the other refuses is itself the failure.
   */
  protected def sameOutcome(
      what: String,
      ours: ByteArrayOutputStream => ZipSink = parallelZip(_),
      reference: ByteArrayOutputStream => ZipSink = new ZipOutputStream(_)
  )(drive: ZipSink => Unit): Unit = {
    def attempt(make: ByteArrayOutputStream => ZipSink): Either[String, Array[Byte]] = {
      val out = new ByteArrayOutputStream
      val w = make(out)
      try {
        drive(w)
        w.finish()
        w.close()
        Right(out.toByteArray)
      } catch { case refused: ZipException => Left(refused.getMessage) }
    }
    (attempt(ours), attempt(reference)) match {
      case (Right(mine), Right(its)) =>
        val _ = assert(
          java.util.Arrays.equals(mine, its),
          s"$what diverged at ${firstDifference(mine, its)}"
        )
      case (Left(mine), Left(its)) =>
        val _ = assert(mine === its, what)
      case (mine, its) =>
        fail(s"$what: one writer refused and the other did not, $mine against $its")
    }
  }

  /** What a writer says when `drive` misuses it, for comparing one writer's wording with another's. */
  protected def refusalFrom(
      make: ByteArrayOutputStream => ZipSink
  )(drive: ZipSink => Unit): String = {
    val w = make(new ByteArrayOutputStream)
    try intercept[ZipException](drive(w)).getMessage
    finally closeQuietly(w)
  }

  /** Closing a writer that has already refused something, which refuses again as it goes. */
  protected def closeQuietly(w: ZipSink): Unit =
    try w.close()
    catch { case _: Throwable => () }

  protected def namesIn(bytes: Array[Byte]): List[String] = {
    val zis = new ZipInputStream(new ByteArrayInputStream(bytes))
    try Iterator.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName).toList
    finally zis.close()
  }

  protected def parallel(out: OutputStream): ZipSink =
    parallelZip(out)
  protected def sequential(out: OutputStream): ZipSink = new ZipOutputStream(out)

  /**
   * Runs `check` over each writer in turn, for the contract tests that hold of both alike. The
   * result is discarded as `foreach` discards one, so a body may end in an assertion.
   */
  protected def bothWriters[U](check: (String, OutputStream => ZipSink) => U): Unit =
    Seq[(String, OutputStream => ZipSink)]("parallel" -> parallel, "sequential" -> sequential)
      .foreach { case (label, make) => check(label, make) }

  /** Drives a writer the way IO.writeEntries does: directory entries, then file entries. */
  protected def writeThrough(
      w: ZipSink,
      dirs: Seq[String],
      files: Seq[(String, Array[Byte], Long)],
      dirTime: Long
  ): Unit = {
    dirs.foreach { name =>
      w.putNextEntry(directoryEntry(name, dirTime))
      w.closeEntry()
    }
    files.foreach { case (name, body, time) =>
      w.putNextEntry(entry(name, body.length, time))
      w.write(body, 0, body.length)
      w.closeEntry()
    }
    w.finish()
    // exercise the documented contract: finish completes the archive, close releases what it holds
    w.close()
  }

  /** What a deflater at the writer's own settings makes of `body`: its compressed size and its crc. */
  protected def deflatedSizes(body: Array[Byte]): (Long, Long) = {
    val d = new Deflater(Deflater.DEFAULT_COMPRESSION, true)
    d.setInput(body)
    d.finish()
    val out = new Array[Byte](body.length + 1024)
    @tailrec def deflatedFrom(n: Int): Int =
      if (d.finished()) n else deflatedFrom(n + d.deflate(out, n, out.length - n))
    val compressed = deflatedFrom(0)
    d.end()
    (compressed.toLong, crcOf(body))
  }

  protected def declaring(name: String, body: Array[Byte]): ZipEntry = {
    val (compressed, crc) = deflatedSizes(body)
    val e = new ZipEntry(name)
    e.setTime(stamp)
    e.setSize(body.length.toLong)
    e.setCompressedSize(compressed)
    e.setCrc(crc)
    e
  }
}
