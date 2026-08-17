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
import java.nio.charset.StandardCharsets
import java.util.zip.{ ZipEntry, ZipException, ZipOutputStream }
import scala.annotation.tailrec
import ZipConstants._

/**
 * What a real `ZipOutputStream` would do, asked at run time rather than restated, so that a JDK
 * disagreeing with its predecessors needs no change to either writer.
 *
 * A probe is built for the call and closed with it, never kept: it holds a `Deflater` and native
 * memory nothing but `end` frees. [[header]] costs a probe archive per entry — 2.5 µs against 490 µs
 * to deflate a class-sized entry — and caching it would mean keying on fields nothing can read.
 */
private[parallel] object ZipReference {

  /**
   * What the reference would write for an entry, and what it settled the entry's own time to. The
   * two extra fields differ once the entry carries file times — the central info-zip field records
   * only the modification time. `madeBy` and `attributes` carry the platform and permissions of an
   * entry that came from a `ZipFile`, which only its central header records.
   */
  final case class Header(
      time: Long,
      dosTime: Long,
      madeBy: Int,
      attributes: Long,
      local: Array[Byte],
      central: Array[Byte],
      refusesCentral: Boolean,
      centralSlack: Int
  )

  /**
   * The dos time and extra fields `ZipOutputStream` would write for this entry, read out of a
   * header it writes for a copy — not derived from what the entry reports, because nothing a
   * subclass can read tells an entry whose times came through `setExtra` or `setLastModifiedTime`
   * from a plain one, while the written header differs across JDKs.
   *
   * The oversized-central-header refusal comes back from here too, as the reference's own verdict:
   * its check runs before it prepends the info-zip timestamp field, so what it writes is 5, 9 or 36
   * bytes longer than what it weighed, and weighing the written header refuses ones it accepts.
   */
  def header(e: ZipEntry, out: ProbeSink): Header = {
    val probe = new ZipEntry(e)
    probe.setMethod(ZipEntry.STORED)
    probe.setSize(0)
    probe.setCompressedSize(0)
    probe.setCrc(0)
    out.reset()
    val z = new ProbeStream(out)
    try {
      z.putNextEntry(probe)
      val localBytes = out.size
      // 22 and later refuse an oversized central header, leaving only the local field to read
      val refused =
        try {
          z.closeEntry()
          z.finish()
          false
        } catch { case _: ZipException => true }
      val b = out.bytes
      val local = extraIn(b, LocBytes, LocNameLengthOffset, LocExtraLengthOffset, at = 0)
      val central =
        if (refused) local
        else extraIn(b, CentralHeaderBytes, CenNameLengthOffset, CenExtraLengthOffset, localBytes)
      // platform and permissions live only in the central header, so a refused one leaves defaults
      val madeBy = if (refused) 0 else u16(b, localBytes + CenMadeByOffset)
      val attributes = if (refused) 0L else u32(b, localBytes + CenAttributesOffset)
      val dos = u32(b, LocTimeOffset)
      // what the reference wrote for the central header, measured rather than recomputed
      val centralBytes = out.size - localBytes - EndRecordBytes
      // asked only of a JDK with a refusal to give, and only where an addition could reach the
      // limit. The probes below reuse the sink, whose bytes have all been read by now
      val slack =
        if (
          refused || centralBytes + MaxCentralAddition <= MaxFieldBytes ||
          oversizedCentralHeaderRefusal.isEmpty
        ) NoCentralLimit
        else centralSlack(e, out)
      Header(probe.getTime, dos, madeBy, attributes, local, central, refused, slack)
    } finally closeQuietly(z)
  }

  /**
   * The fewest bytes added to this entry's central header that the reference refuses, or
   * [[NoCentralLimit]] where it takes the most a writer could add. The zip64 field is the one thing
   * [[header]]'s probe cannot carry, its sizes and offset all being zero, so it is asked apart — in
   * comment bytes, which the reference's check weighs exactly as it weighs a wider extra field.
   * Widest addition first: where that fits, no narrower one can fail.
   */
  private def centralSlack(e: ZipEntry, out: ProbeSink): Int =
    if (!refusesCentralWith(e, MaxCentralAddition, out)) NoCentralLimit
    else
      // the narrowest addition it refuses, the widest one already having been refused above
      CentralAdditions.init
        .find(added => refusesCentralWith(e, added, out))
        .getOrElse(MaxCentralAddition)

  private def refusesCentralWith(e: ZipEntry, added: Int, out: ProbeSink): Boolean = {
    val probe = new ZipEntry(e)
    probe.setMethod(ZipEntry.STORED)
    probe.setSize(0)
    probe.setCompressedSize(0)
    probe.setCrc(0)
    val comment = Option(e.getComment).getOrElse("")
    // one byte of utf-8 each, so the header grows by exactly `added`. Within what `setComment`
    // takes, since this is asked only of an entry the reference did not already refuse
    probe.setComment(comment + CommentFiller * added)
    out.reset()
    val z = new ProbeStream(out)
    try {
      z.putNextEntry(probe)
      z.closeEntry()
      z.finish()
      false
    } catch { case _: ZipException => true }
    finally closeQuietly(z)
  }

  /**
   * Whether the reference puts this entry's sizes in its local header rather than in a descriptor.
   * Not derivable from the entry — the reference is asked: bit 3 of the header flag it writes is
   * the answer. Only an entry carrying all three sizes reaches this.
   */
  def declaresSizes(e: ZipEntry, out: ProbeSink): Boolean =
    e.getSize != Unset && e.getCompressedSize != Unset && e.getCrc != Unset && {
      val probe = new ZipEntry(e)
      probe.setSize(0)
      probe.setCrc(0)
      out.reset()
      val z = new ProbeStream(out)
      try {
        z.putNextEntry(probe)
        (u16(out.bytes, LocFlagOffset) & DescriptorFlag) == 0
      } finally closeQuietly(z)
    }

  /**
   * Fills a stored entry's sizes in from each other, as the reference does, and leaves them on the
   * entry where a caller can read them back. `setCompressedSize` is the only way to do that from
   * outside, and unlike the reference's own assignment it records that the caller set it — which
   * the reference would go on to read if that same entry were later written again as deflated.
   */
  def fillStoredSizes(e: ZipEntry): Unit = {
    val declared = e.getSize
    val declaredCompressed = e.getCompressedSize
    if (declared != Unset && declaredCompressed != Unset && declared != declaredCompressed)
      throw new ZipException("STORED entry where compressed != uncompressed size")
    val size = if (declared == Unset) declaredCompressed else declared
    val compressed = if (declaredCompressed == Unset) declared else declaredCompressed
    if (size == Unset || e.getCrc == Unset)
      throw new ZipException("STORED entry missing size, compressed size, or crc-32")
    // written back only where the reference fills one in from the other, keeping the record
    // `setCompressedSize` leaves to the case it cannot be kept out of. The size guard is not the
    // same test: a compressed size may be negative, where `setSize` refuses what the reference took
    if (declared == Unset && size >= 0) e.setSize(size)
    if (declaredCompressed == Unset) e.setCompressedSize(compressed)
  }

  /**
   * Puts a setting through a reference, so one it refuses is refused in this JDK's own words — they
   * move, the comment refusal ending in a full stop before 21 and not after. Built for the call: it
   * holds a deflater, and settings change once where thousands of entries are written.
   */
  def validate(setting: ZipOutputStream => Unit): Unit = {
    val z = new ProbeStream(new ByteArrayOutputStream(ProbeArchiveBytes))
    try setting(z)
    finally closeQuietly(z)
  }

  /**
   * Whether `setComment(null)` clears a comment already set or leaves it where it was. Measured, 8 and
   * 11 assign only a comment that is not null, so null leaves the last one standing; 21 and later assign
   * either way and null clears it. Read off an end record rather than restated, so a JDK that moves again
   * needs no change here.
   */
  lazy val nullCommentClears: Boolean = {
    val out = new ByteArrayOutputStream(ProbeArchiveBytes)
    val z = new ProbeStream(out)
    try {
      z.setComment(CommentProbe)
      z.setComment(null)
      z.finish()
    } finally closeQuietly(z)
    // an archive holding no entries is its end record, so anything past it is the comment
    out.size == EndRecordBytes
  }

  /**
   * This JDK's refusal of a central header past 64 KB, or nothing where it writes one anyway:
   * 8-21 do, 22+ refuse. Probed once so the wording matches the running JDK's.
   */
  lazy val oversizedCentralHeaderRefusal: Option[String] = {
    val roomForName = MaxFieldBytes - CentralHeaderBytes
    val overflowingLength = roomForName / MaxUtf8BytesPerChar + 1
    val wideName = MaxUtf8Char * overflowingLength
    val e = new ZipEntry(wideName)
    e.setTime(0L)
    val out = new ByteArrayOutputStream(ProbeArchiveBytes)
    val z = new ProbeStream(out)
    try {
      z.putNextEntry(e)
      z.closeEntry()
      z.finish()
      None
    } catch { case refused: ZipException => Some(refused.getMessage) }
    finally closeQuietly(z)
  }

  /**
   * The room a reference gives zlib for one `deflate`, which reaches the bytes: a block settled
   * within that room is framed by it, so a window short of the reference's diverges.
   *
   * `DeflaterOutputStream`'s buffer is package private, so it is measured as the widest write a
   * reference makes while deflating, given a body big enough to fill it. Too small a body
   * under-reports silently, so it doubles from [[ProbeBodyBytes]] until the measurement stops
   * growing — which resolves 512 through 1 MB exactly, where a fixed 128 KB body reports 114758 for
   * anything wider. 512 on 8 to 26, settled in two passes.
   */
  lazy val deflateBuffer: Int = {
    @tailrec def settle(body: Int, widest: Int): Int =
      if (body > MaxProbeBodyBytes / 2) widest
      else {
        val again = widestDeflateWrite(body * 2)
        if (again > widest) settle(body * 2, again) else widest
      }
    val measured = settle(ProbeBodyBytes, widestDeflateWrite(ProbeBodyBytes))
    if (measured > 0) measured else FallbackDeflateBuffer
  }

  private def widestDeflateWrite(bodyBytes: Int): Int = {
    val sink = new WidestWrite
    val z = new ProbeStream(sink)
    val body = new Array[Byte](bodyBytes)
    new java.util.Random(0).nextBytes(body)
    try {
      val e = new ZipEntry("p")
      e.setTime(0L)
      z.putNextEntry(e)
      val widest = sink.measuring(z.write(body, 0, body.length))
      z.closeEntry()
      widest
    } finally closeQuietly(z)
  }

  /** Records the widest single write it is given, which is what reveals the buffer behind it. */
  private final class WidestWrite extends OutputStream {
    private var watching = false
    private var widest = 0

    /** The widest write `writing` makes: those around it are a name or an extra field, not a buffer. */
    def measuring(writing: => Unit): Int = {
      watching = true
      writing
      watching = false
      widest
    }

    override def write(b: Int): Unit = ()
    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      if (watching && len > widest) widest = len
  }

  private def extraIn(
      b: Array[Byte],
      headerBytes: Int,
      nameLengthOffset: Int,
      extraLengthOffset: Int,
      at: Int
  ): Array[Byte] = {
    val nameLength = u16(b, at + nameLengthOffset)
    val extraLength = u16(b, at + extraLengthOffset)
    // shared where there is nothing to carry: a header outlives its entry
    if (extraLength == 0) Array.emptyByteArray
    else {
      val extraAt = at + headerBytes + nameLength
      b.slice(extraAt, extraAt + extraLength)
    }
  }

  private final class ProbeStream(out: OutputStream)
      extends ZipOutputStream(out, StandardCharsets.UTF_8) {
    def endDeflater(): Unit = `def`.end()
  }

  private def closeQuietly(z: ProbeStream): Unit =
    try z.close()
    catch { case _: ZipException => () }
    finally z.endDeflater()

  /**
   * Where a probe writes, kept by the writer that asks rather than made for each question: the sink
   * and the copy taken out of it are a quarter of what a probe costs, and neither has to be new.
   */
  private[parallel] final class ProbeSink extends ByteArrayOutputStream(ProbeArchiveBytes) {

    /** The bytes written so far, in place. Longer than [[size]], so offsets are read against that. */
    def bytes: Array[Byte] = buf
  }

  /** Where a central header has room for anything a writer may add to it. */
  private[parallel] final val NoCentralLimit = Int.MaxValue

  /**
   * What a writer adds to a central header past the entry's own: a zip64 field, holding one, two or
   * three of the size, compressed size and offset it moves, with the two byte id and length in front.
   */
  private final val CentralAdditions =
    Array(1, 2, 3).map(fields => ExtraHeaderBytes + fields * Zip64FieldBytes)

  private final val MaxCentralAddition = CentralAdditions(CentralAdditions.length - 1)

  /** A byte of comment, for asking a central header question in the units the reference weighs. */
  private final val CommentFiller = "x"

  /** Enough for any header a probe writes, since none of them carries a body. */
  private final val ProbeArchiveBytes = 256

  /** A comment for [[nullCommentClears]] to look for. Any non-empty one does: its presence is the answer. */
  private final val CommentProbe = "c"

  /**
   * Where the search for the reference's deflate buffer starts. Past zlib's own 32 KB window: under
   * that the reference emits nothing while being written to, having accumulated rather than
   * deflated, and a search begun there would settle on a couple of bytes.
   */
  private final val ProbeBodyBytes = 128 * 1024

  /**
   * Where the search gives up rather than doubling into an overflowing `Int`. Far past any JDK, so
   * reaching it means a reference unlike all of them, measured short rather than not at all: the
   * widest write seen so far is what is kept.
   */
  private final val MaxProbeBodyBytes = 16 * 1024 * 1024

  /** Where the reference's own buffer cannot be measured. Every JDK from 8 to 26 uses this. */
  private final val FallbackDeflateBuffer = 512

  /** What it takes to overflow a central header: the widest character UTF-8 encodes, repeated. */
  private final val MaxUtf8BytesPerChar = 3
  private final val MaxUtf8Char = "中"
}
