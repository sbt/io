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
import java.nio.file.attribute.FileTime
import java.security.MessageDigest
import java.util.concurrent.TimeUnit
import java.util.jar.JarOutputStream
import java.util.zip.{ CRC32, Deflater, ZipEntry, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.Hash
import sbt.io.ZipTestSupport.{ firstDifference, hasSignature, sameBytes }
import ZipConstants.{
  CentralHeaderBytes,
  MaxFieldBytes,
  Zip64EndSig,
  Zip64ExtraId,
  Zip64LocatorSig
}

/**
 * What happens where a field overflows: sizes and offsets past 4 GB, a name or an extra field past
 * the 64 KB a header length holds, and an entry count past the 65535 the end record holds.
 */
class ParallelZipLimitsSpec extends AnyFunSuite with ParallelZipSupport {

  /** How much of an archive's tail is kept, for a message more useful than a hash mismatch. */
  private final val TailBytes = 1024

  /** What "中" takes in utf-8, which is how a name reaches past a length field that counts bytes. */
  private final val Utf8BytesPerWideChar = 3

  /** `chunk` over and over until `size` bytes have gone by, which is how a 4 GB fixture is made. */
  private def perChunk(size: Long, chunk: Array[Byte])(use: Array[Byte] => Unit): Unit =
    (0L until size by chunk.length.toLong).foreach(_ => use(chunk))

  /**
   * Compares archives too large to hold: every byte through a digest, and the tail kept so that a
   * divergence in the records that carry zip64 says something more useful than a hash mismatch.
   */
  private final class Ends extends OutputStream {
    private val digest = MessageDigest.getInstance("SHA-256")
    private val kept = new Array[Byte](TailBytes) // the newest bytes, oldest first, right-aligned
    private var count = 0L

    override def write(b: Int): Unit = write(Array(b.toByte), 0, 1)

    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      digest.update(b, off, len)
      val arriving = math.min(len, kept.length) // only the last of these can survive
      System.arraycopy(kept, arriving, kept, 0, kept.length - arriving) // the older ones slide down
      System.arraycopy(b, off + len - arriving, kept, kept.length - arriving, arriving)
      count += len
    }

    def tail: Array[Byte] = {
      val n = math.min(count, kept.length.toLong).toInt
      kept.slice(kept.length - n, kept.length)
    }

    private def fingerprint: String = Hash.toHex(digest.digest())

    def mustMatch(reference: Ends): Unit = {
      assert(
        count === reference.count,
        s"the archives are different lengths, and the tails differ at " +
          firstDifference(tail, reference.tail)
      )
      assert(
        fingerprint === reference.fingerprint,
        s"same length, different bytes; the tails differ at ${firstDifference(tail, reference.tail)}"
      )
      ()
    }

    /** Whether the tail carries a record signature or an extra field id. */
    def contains(id: Int, twoBytes: Boolean): Boolean =
      hasSignature(tail, id, if (twoBytes) 2 else 4)
  }

  test(
    "a stored entry whose sizes pass 4 GB gets the reference's zip64 local header, byte for byte"
  ) {
    // the only way to a zip64 field in a *local* header: the sizes have to be in it to overflow, which a
    // data descriptor's are not. 4 GiB of zeros, stored, so both sizes pass what 32 bits hold.
    val size = 0x100000000L
    val chunk = new Array[Byte](1 << 16)
    val crc = new CRC32
    perChunk(size, chunk)(crc.update(_, 0, chunk.length))
    def archive(make: OutputStream => ZipSink): Ends = {
      val out = new Ends
      val w = make(out)
      val e = new ZipEntry("huge.bin")
      e.setTime(stamp)
      e.setMethod(ZipEntry.STORED)
      e.setSize(size)
      e.setCompressedSize(size)
      e.setCrc(crc.getValue)
      w.putNextEntry(e)
      perChunk(size, chunk)(w.write(_, 0, chunk.length))
      w.closeEntry()
      w.finish()
      w.close()
      out
    }
    val ours = archive(parallelZip(_))
    val reference = archive(new ZipOutputStream(_))
    ours.mustMatch(reference)
    assert(ours.contains(Zip64EndSig.toInt, twoBytes = false), "no zip64 end record")
  }

  test("an entry name past 64 KB of utf-8 goes the way the reference takes it") {
    // ZipEntry caps names at 65535 *characters*, so a multi-byte name reaches past the 65535 bytes the
    // header records its length in. What happens then changed: measured, 8, 11 and 21 wrap the length and
    // write an archive nothing can read, and 22 and later refuse the archive as the directory goes down.
    val name = "\u4e2d" * 30000 // 90000 utf-8 bytes
    assert(name.getBytes("UTF-8").length === 90000)
    Seq[(String, () => ZipEntry)](
      "deflated" -> (() => entry(name)),
      "stored" -> (() => directoryEntry(name + "/"))
    ).foreach { case (what, make) =>
      sameOutcome(what) { w =>
        w.putNextEntry(make())
        w.closeEntry()
      }
    }
  }

  test("a central header only the timestamp field pushes past 64 KB goes the reference's way") {
    // the reference weighs its 64 KB check before prepending the info-zip field, so what it writes is
    // 9 bytes longer than what it weighed and a header of 65527 to 65535 bytes is one it writes but a
    // writer weighing the written header would refuse. Only an entry carrying file times has that field
    val atLimit = (MaxFieldBytes - CentralHeaderBytes) / Utf8BytesPerWideChar
    Seq(atLimit - 3, atLimit - 1, atLimit, atLimit + 1).foreach { chars =>
      Seq(true, false).foreach { withFileTime =>
        def make(): ZipEntry = {
          val e = entry("中" * chars)
          if (withFileTime) e.setLastModifiedTime(FileTime.from(0L, TimeUnit.MILLISECONDS))
          e
        }
        sameOutcome(s"${chars * Utf8BytesPerWideChar} name bytes, file time $withFileTime") { w =>
          w.putNextEntry(make())
          w.write(oneByte, 0, oneByte.length)
          w.closeEntry()
        }
      }
    }
  }

  test("an entry comment past 64 KB of utf-8 goes the way the reference takes it") {
    // the one length the reference cuts rather than wraps: a comment is written short with its length
    // pinned at 65535, where a name past 65535 bytes is written whole under a wrapped length. 22 and
    // later refuse it, so what there is to agree on moves from the bytes to the refusal
    val comment = "中" * 30000 // 90000 utf-8 bytes
    def commented(): Option[ZipEntry] = {
      val e = entry("a.txt")
      try { e.setComment(comment); Some(e) }
      catch { case _: IllegalArgumentException => None }
    }
    assume(commented().isDefined, "this JDK refuses a comment past what a central header records")

    sameOutcome(s"a ${comment.length} character comment") { w =>
      w.putNextEntry(commented().get)
      w.write(oneByte, 0, oneByte.length)
      w.closeEntry()
    }
  }

  test("an extra field the jar magic pushes past 64 KB is refused as the reference refuses it") {
    // A full field plus the four magic bytes is four over what the header records. Both writers stamp the
    // field back onto the entry, so `ZipEntry.setExtra` is what refuses the result — needs a full 0xffff
    // field, which JDK 26 will not build: measured, it caps setExtra at 0xffd0, so it is unreachable there.
    val full = new Array[Byte](0xffff)
    full(0) = 0x11.toByte // one well-formed field, not the jar magic, filling the whole array
    full(1) = 0x22.toByte
    full(2) = ((0xffff - 4) & 0xff).toByte
    full(3) = (((0xffff - 4) >>> 8) & 0xff).toByte
    def filled(): Option[ZipEntry] = {
      val e = entry("a.txt")
      try { e.setExtra(full); Some(e) }
      catch { case _: IllegalArgumentException => None }
    }
    assume(
      filled().isDefined,
      "this JDK's setExtra caps the extra field below what the prepend needs"
    )

    def refusal(open: ZipEntry => Unit): String =
      intercept[IllegalArgumentException](open(filled().get)).getMessage
    val reference = refusal(new JarOutputStream(new ByteArrayOutputStream).putNextEntry(_))
    val ours = refusal(parallelJar(new ByteArrayOutputStream).putNextEntry(_))
    assert(
      ours === reference,
      "the refusal has to be the one ZipEntry.setExtra gives the reference"
    )
  }

  test(
    "an entry whose uncompressed size passes 4 GB gets the reference's zip64 fields, byte for byte"
  ) {
    // 4 GB of zeros, so the archive stays small while the uncompressed size runs past what its 32 bit
    // field holds — which moves it into a zip64 extra field and widens the data descriptor to 64 bits
    val size = 0x100000000L
    def archive(make: OutputStream => ZipSink): Ends = {
      val out = new Ends
      val w = make(out)
      val e = new ZipEntry("huge.bin")
      e.setTime(stamp)
      e.setSize(size) // past DefaultMaxEntryBytes, so ours streams it rather than holding it
      w.putNextEntry(e)
      val chunk = new Array[Byte](1 << 16)
      perChunk(size, chunk)(w.write(_, 0, chunk.length))
      w.closeEntry()
      w.finish()
      w.close()
      out
    }
    val ours = archive(parallelZip(_))
    val reference = archive(new ZipOutputStream(_))
    ours.mustMatch(reference)
    assert(
      ours.contains(Zip64ExtraId, twoBytes = true),
      "no zip64 extra field in the central record"
    )
  }

  test("an archive whose offsets pass 4 GB gets the reference's zip64 records, byte for byte") {
    // an archive can pass 4 GB across entries none of which does, which moves the second entry's local
    // header offset and the directory's own into zip64 records. At level 0 deflate output is its input plus
    // a block header, so the offset gets there without 4 GB of incompressible input to make.
    def archive(make: OutputStream => ZipSink): Ends = {
      val out = new Ends
      val w = make(out)
      w.setLevel(Deflater.NO_COMPRESSION)
      w.putNextEntry(entry("huge.bin", 0)) // undeclared size, so ours streams it
      val chunk = new Array[Byte](1 << 16)
      perChunk(0x100000000L, chunk)(w.write(_, 0, chunk.length))
      w.closeEntry()
      writeOne(w, "after.txt") // its offset is past 4 GB, and so is the directory's
      w.finish()
      w.close()
      out
    }
    val ours = archive(out => holdingUpTo(out, 1024L))
    val reference = archive(new ZipOutputStream(_))
    ours.mustMatch(reference)
    assert(ours.contains(Zip64EndSig.toInt, twoBytes = false), "no zip64 end record")
    assert(ours.contains(Zip64LocatorSig.toInt, twoBytes = false), "no zip64 end record locator")
  }

  test(
    "the entry count where the reference reaches for zip64 is where this writer does, byte for byte"
  ) {
    // directory entries keep it cheap: nothing to deflate. 65534 is the most a plain end record counts and
    // 65535 is where the reference starts writing a zip64 one beside it, so both sides of that are here.
    Seq(0xfffe -> false, 0xffff -> true, 0x10000 -> true).foreach { case (count, zip64) =>
      def drive(w: ZipSink): Unit =
        (0 until count).foreach { i =>
          w.putNextEntry(directoryEntry(f"d$i%05d/"))
          w.closeEntry()
        }
      val ours = archive(parallelZip(_))(drive)
      sameBytes(s"$count entries", ours, archive(new ZipOutputStream(_))(drive), "the reference")
      assert(
        hasSignature(ours, Zip64EndSig.toInt) === zip64,
        s"$count entries: zip64 end record ${if (zip64) "missing"
          else "written where the reference wrote none"}"
      )
    }
  }

  test("an archive past 65535 entries gets the reference's end records, byte for byte") {
    val count = 0x10000 + 1 // one past what the 16 bit entry count holds
    def drive(w: ZipSink): Unit = {
      (0 until count).foreach(i => w.putNextEntry(directoryEntry(s"d$i/")))
      w.closeEntry()
    }
    val ours = archive(parallelZip(_))(drive)
    sameBytes(
      "an archive past 65535 entries",
      ours,
      archive(new ZipOutputStream(_))(drive),
      "the reference"
    )
    assert(hasSignature(ours, Zip64EndSig.toInt), "no zip64 end record past 65535 entries")
  }
}
