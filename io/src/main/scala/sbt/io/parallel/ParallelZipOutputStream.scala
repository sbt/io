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

import java.io.{ BufferedOutputStream, InterruptedIOException, IOException, OutputStream }
import java.nio.charset.StandardCharsets
import java.util.{ ArrayDeque, Arrays }
import java.util.concurrent.{ ExecutionException, FutureTask }
import java.util.zip.{ CRC32, Deflater, ZipEntry, ZipException }
import scala.annotation.tailrec
import scala.collection.mutable.{ HashSet, ListBuffer }
import scala.concurrent.ExecutionContext
import ZipConstants._

/**
 * A zip writer that deflates entries in parallel while producing byte-for-byte identical output to
 * `ZipOutputStream`. Where JDK versions disagree the answer is read from [[ZipReference]] at run
 * time, so a new JDK needs no change here.
 *
 * Not thread safe. Names and comments are always UTF-8, as `IO.defaultCharset` is.
 *
 * @param parallelism how many entries may be in flight, not a thread count; each holds a `Deflater`
 *                    and around 256 KB of native zlib state
 * @param ec where deflation is offered; the writing thread deflates whatever it has not started
 */
private[sbt] class ParallelZipOutputStream(
    to: OutputStream,
    parallelism: Int
)(implicit ec: ExecutionContext)
    extends ZipSink {
  import ParallelZipOutputStream._

  require(parallelism >= 1, "parallelism must be at least 1, was " + parallelism)

  private val out = new BufferedOutputStream(to, SinkBufferBytes)
  private var written = 0L
  private var locoff = 0L

  private val xentries = ListBuffer.empty[XEntry]
  private val names = HashSet.empty[String]

  private var current: Option[XEntry] = None
  private var finished = false

  private var level = Deflater.DEFAULT_COMPRESSION
  private var method = ZipEntry.DEFLATED
  private var comment: Array[Byte] = Array.emptyByteArray

  private val held = new HeldEntry(takeBuffer(_))

  /** [[maxEntryBytes]], capped at what an `Int` of held bytes holds. Past it an entry is streamed. */
  private def holdCeiling: Long = math.min(maxEntryBytes, Int.MaxValue.toLong)

  private val oneByte = new Array[Byte](1)

  // one sink for every question, since a reference is asked one per entry
  private val probeSink = new ZipReference.ProbeSink

  private val inFlight = new ArrayDeque[Pending]
  private var inFlightBytes = 0L

  private var streaming = false
  private val crc = new CRC32
  private var deflating: Option[Deflating] = None

  private val freeBuffers = new ArrayDeque[Array[Byte]]
  private var freeBytes = 0L
  private val freeDeflaters = new ArrayDeque[Deflater]
  private val recycled = math.min(parallelism, MaxRecycled)
  // a window's worth of blocks, since an entry is held in as many as it takes: sized per entry
  // instead, the list would drop all but a handful. Bytes are bounded by the window either way
  private val freeSlots =
    math.max(recycled * BuffersPerEntry + SpareBuffers, (WindowBytes / HoldBlockBytes).toInt)

  // ── what a subclass may change ───────────────────────────────────────

  /** Overridden by the jar writer to stamp a jar's first entry before its header is probed. */
  protected def stamp(e: ZipEntry): Unit = ()

  /** Fixed rather than offered to a caller; overridable only so a test can drive either path. */
  protected def windowBytes: Long = WindowBytes

  protected def maxEntryBytes: Long = MaxEntryBytes

  /** Overridable so a suite can hand out a deflater it is able to block inside. */
  protected def newDeflater(atLevel: Int): Deflater = new Deflater(atLevel, true)

  // ── entry lifecycle ──────────────────────────────────────────────────

  def putNextEntry(e: ZipEntry): Unit = {
    if (finished) throw new ZipException("the archive is finished")
    // before the entry is looked at, as `JarOutputStream` stamps it: one refused below has still
    // spent the jar's one magic field
    stamp(e)
    if (current.isDefined) closeEntry()
    if (e.getMethod == Unset) e.setMethod(method)
    val entryMethod = e.getMethod
    val stored = entryMethod == ZipEntry.STORED
    if (!stored && entryMethod != ZipEntry.DEFLATED)
      throw new ZipException("unsupported compression method")
    if (stored) ZipReference.fillStoredSizes(e)
    claim(e.getName)
    val sizesInHeader = stored || ZipReference.declaresSizes(e, probeSink)
    val reference = ZipReference.header(e, probeSink)
    // the reference stamps an entry carrying no time of its own, and only it can tell whether this
    // is one: the field it reads is not the one `getTime` reports
    if (e.getTime != reference.time) e.setTime(reference.time)
    val entry = new XEntry(
      name = utf8(e.getName),
      comment = commentBytes(e),
      reference = reference,
      method = entryMethod,
      flag = Utf8Flag | (if (sizesInHeader) 0 else DescriptorFlag),
      crc = e.getCrc,
      csize = e.getCompressedSize,
      size = e.getSize,
      offset = 0L
    )
    current = Some(entry)
    // level 0 streams: zlib frames a stored block from what it has been handed, so only passing the
    // caller's writes through unchanged reproduces the reference's block boundaries
    val uncompressed = entryMethod == ZipEntry.DEFLATED && level == Deflater.NO_COMPRESSION
    if (sizesInHeader || uncompressed || e.getSize > maxEntryBytes) startStreaming(entry)
    else held.open()
  }

  override def write(b: Int): Unit = {
    oneByte(0) = b.toByte
    write(oneByte, 0, 1)
  }

  /** Synchronized because the reference synchronizes this method, and only this one. */
  override def write(b: Array[Byte], off: Int, len: Int): Unit = synchronized {
    if (off < 0 || len < 0 || off > b.length - len) throw new IndexOutOfBoundsException
    else if (len == 0) ()
    else if (streaming) writeStreaming(b, off, len)
    else if (!held.isOpen) throw new ZipException("no current ZIP entry")
    else if (held.bytes.toLong + len > holdCeiling || held.writes == MaxHeldWrites) {
      // decided before the copy, or the peak this bounds would be whatever arrived in one array.
      // What was held reaches the deflater as its own call, which is safe only because a held entry
      // is never at level 0: levels 1 to 9 frame blocks from input, not from calls
      current.foreach(startStreaming)
      writeStreaming(b, off, len)
    } else held.put(b, off, len)
  }

  def closeEntry(): Unit =
    current.foreach { x =>
      if (streaming) {
        // cleared only once the entry is accepted, as the reference clears it: one it refused stays
        // open, so a caller carrying on is refused against what it has written since
        closeStreaming(x)
        streaming = false
        current = None
      } else if (held.isOpen) {
        current = None
        submit(x)
      }
    }

  def finish(): Unit =
    if (!finished) {
      if (current.isDefined) closeEntry()
      drainAll()
      val start = written
      xentries.foreach(writeCEN)
      writeEND(start, written - start)
      out.flush()
      finished = true
    }

  override def flush(): Unit = {
    drainAll()
    out.flush()
  }

  override def close(): Unit =
    try finish()
    finally
      try {
        deflating.foreach(_.deflater.end())
        deflating = None
        inFlight.forEach(_.endDeflater())
        inFlight.clear()
        inFlightBytes = 0L
        endFreeDeflaters()
        freeBuffers.clear()
        freeBytes = 0L
      } finally out.close()

  // ── settings ─────────────────────────────────────────────────────────

  def setLevel(level: Int): Unit = {
    ZipReference.validate(_.setLevel(level))
    if (level != this.level) {
      // a held entry has no deflater for `setLevel` to reach, so it streams first: what it holds is
      // replayed at the level it was held at, leaving the same bytes deflated as the reference's
      if (held.isOpen) current.foreach(startStreaming)
      deflating = deflating.map { open =>
        open.deflater.setLevel(level)
        // zlib settles the change by flushing a block, and the room given for that reaches the
        // output at every level: measured, 64 KB diverges where the reference's own 512 matches
        open.levelChanged(new Array[Byte](ZipReference.deflateBuffer))
      }
      // made at the level being left behind, so none will deflate like one made at this one
      endFreeDeflaters()
      this.level = level
    }
  }

  def setMethod(method: Int): Unit = {
    ZipReference.validate(_.setMethod(method))
    this.method = method
  }

  def setComment(comment: String): Unit =
    Option(comment) match {
      case Some(text) =>
        ZipReference.validate(_.setComment(text))
        this.comment = utf8(text)
      case None => if (ZipReference.nullCommentClears) this.comment = Array.emptyByteArray
    }

  // ── streaming path ──────────────────────────────────────────────────

  /** Everything queued is appended first: this entry's header goes down at the current offset. */
  private def startStreaming(open: XEntry): Unit = {
    drainAll()
    // emptied before the header goes down, so a failure writing that header leaves nothing held: an
    // entry both open and held is one `closeEntry` would go on to submit and write a second time
    val replay = held.take()
    val x = open.at(written)
    current = Some(x)
    writeLOC(x)
    streaming = true
    crc.reset()
    deflating =
      // the reference's own buffer: a `deflate` given more room drains more of zlib's pending
      // output, which is what a later `setLevel` on this entry would flush a block from
      if (x.method == ZipEntry.DEFLATED)
        Some(new Deflating(takeDeflater(), new Array[Byte](ZipReference.deflateBuffer), level))
      else None
    replay.replayTo(writeStreaming)
    giveBlocks(replay.blocks)
  }

  private def writeStreaming(b: Array[Byte], off: Int, len: Int): Unit = {
    deflating match {
      case Some(open) =>
        // an entry the reference refused keeps the deflater it finished, which takes no more input:
        // `deflate` would return nothing while `needsInput` stayed false and the loop would not end
        if (open.deflater.finished()) throw new IOException("write beyond end of stream")
        open.deflater.setInput(b, off, len)
        drainDeflater(open, () => open.deflater.needsInput())
      case None =>
        // counted before it is refused, as the reference counts it
        written += len
        if (current.exists(written - locoff > _.size))
          throw new ZipException("attempt to write past end of STORED entry")
        out.write(b, off, len)
    }
    crc.update(b, off, len)
  }

  @tailrec
  private def drainDeflater(open: Deflating, done: () => Boolean): Unit =
    if (!done()) {
      val n = open.deflater.deflate(open.buffer, 0, open.buffer.length)
      if (n > 0) writeBytes(open.buffer, 0, n)
      drainDeflater(open, done)
    }

  private def closeStreaming(x: XEntry): Unit = {
    val recorded =
      deflating match {
        case Some(open) =>
          open.deflater.finish()
          drainDeflater(open, () => open.deflater.finished())
          val settled =
            settleStreamed(x, open.deflater.getBytesRead, open.deflater.getBytesWritten)
          // handed back only once the entry is accepted, so closing a refused one re-runs the check
          giveDeflater(open.deflater, open.level)
          deflating = None
          settled
        case None =>
          val size = written - locoff
          if (x.size != size) throw wrongSize("size", x.size, size)
          // the reference spells the label in lower case on this path and in upper on the other
          if (x.crc != crc.getValue) throw wrongCrc("crc-32", x.crc, crc.getValue)
          x
      }
    val _ = xentries += recorded
  }

  private def settleStreamed(x: XEntry, size: Long, csize: Long): XEntry =
    if ((x.flag & DescriptorFlag) == 0) {
      if (x.size != size) throw wrongSize("size", x.size, size)
      if (x.csize != csize) throw wrongSize("compressed size", x.csize, csize)
      if (x.crc != crc.getValue) throw wrongCrc("CRC-32", x.crc, crc.getValue)
      x
    } else {
      val settled = x.settled(crc.getValue, csize, size)
      writeEXT(settled)
      settled
    }

  /** The reference's wording for a size that did not turn out to be what the entry declared. */
  private def wrongSize(what: String, declared: Long, got: Long): ZipException =
    new ZipException(
      "invalid entry " + what + " (expected " + declared + " but got " + got + " bytes)"
    )

  private def wrongCrc(label: String, declared: Long, got: Long): ZipException =
    new ZipException(
      "invalid entry " + label + " (expected 0x" + java.lang.Long.toHexString(declared) +
        " but got 0x" + java.lang.Long.toHexString(got) + ")"
    )

  // ── parallel (buffered) path ─────────────────────────────────────────

  private def submit(x: XEntry): Unit = {
    // out of the holder before anything else: from here the blocks are this entry's alone, and the
    // writing thread has no way back to them until the deflating one hands them over
    val taken = held.take()
    // drained before anything is taken, so this entry gets what the appended one just handed back
    makeRoomFor(taken.capacity)
    // before the deflater, so a heap that cannot take it fails with no deflater to strand
    val bound = math.min(deflatedBound(taken.bytes.toLong), Int.MaxValue.toLong).toInt
    val into = takeBuffer(bound)
    // still the level this entry was opened at: `setLevel` moves a held entry to the streaming path
    val entryLevel = level
    val entryDeflater = takeDeflater()
    // a window wider than the reference's is safe here and nowhere else: this entry is deflated
    // whole and on its own, so no level change lands part way through it
    val window = DeflateBlockBytes
    def deflate(): Deflated =
      deflateInto(entryDeflater, taken.blocks, taken.used, taken.bytes, into, window)
    val task = new FutureTask[Deflated](() => deflate())
    // a submission that was refused was never accepted, and so never runs: this entry reaches
    // neither the queue `close` sweeps nor a deflating thread, and hands back what it holds here
    try ec.execute(task)
    catch {
      case failed: Throwable =>
        giveBlocks(taken.blocks)
        giveBuffer(into)
        entryDeflater.end()
        throw failed
    }
    val pending = new Pending(x, task, taken.blocks, taken.capacity, entryDeflater, entryLevel)
    inFlight.addLast(pending)
    inFlightBytes += taken.capacity
  }

  private def drainOne(): Unit = {
    // left queued until its result is in hand, so a failed deflation still has `close` to end it
    val p = inFlight.getFirst
    val deflated = await(p.claimed())
    val _ = inFlight.removeFirst()
    inFlightBytes -= p.capacity
    giveBlocks(p.input)
    giveDeflater(p.deflater, p.level)
    append(p.entry, deflated)
    giveBuffer(deflated.bytes)
  }

  @tailrec
  private def drainAll(): Unit =
    if (!inFlight.isEmpty) {
      drainOne()
      drainAll()
    }

  @tailrec
  private def makeRoomFor(capacity: Long): Unit =
    if (
      !inFlight.isEmpty &&
      (inFlight.size >= parallelism || inFlightBytes + capacity > windowBytes)
    ) {
      drainOne()
      makeRoomFor(capacity)
    }

  private def giveBlocks(taken: Array[Array[Byte]]): Unit = taken.foreach(giveBuffer)

  private def append(x: XEntry, deflated: Deflated): Unit = {
    val placed =
      x.placed(written, deflated.crc, deflated.csize, deflated.size)
    writeLOC(placed)
    writeBytes(deflated.bytes, 0, placed.csize.toInt)
    writeEXT(placed)
    val _ = xentries += placed
  }

  // ── recycling ────────────────────────────────────────────────────────

  /**
   * A buffer at least this long from what an earlier entry handed back, but not so much wider that
   * allocating would have been better: the window charges an entry for what its blocks measure.
   */
  private def takeBuffer(atLeast: Int): Array[Byte] = {
    val most = MaxFitFactor.toLong * rounded(atLeast)
    val each = freeBuffers.iterator
    @tailrec def firstFitting(): Array[Byte] =
      if (!each.hasNext) new Array[Byte](rounded(atLeast))
      else {
        val b = each.next()
        if (b.length < atLeast || b.length > most) firstFitting()
        else {
          each.remove()
          freeBytes -= b.length
          b
        }
      }
    firstFitting()
  }

  /** What the free list holds, for a suite pinning that a refused entry handed its buffers back. */
  private[parallel] def recycledBuffers: Int = freeBuffers.size

  /**
   * Kept within the bytes the window already bounds, and to no more buffers than are worth scanning.
   * The oldest goes to make room: turning the arriving one away would freeze the list as it stands.
   */
  private def giveBuffer(b: Array[Byte]): Unit = {
    dropOldestUntilRoom(b.length)
    if (freeBytes + b.length <= windowBytes) {
      freeBuffers.addFirst(b)
      freeBytes += b.length
    }
  }

  @tailrec
  private def dropOldestUntilRoom(arriving: Int): Unit =
    if (
      !freeBuffers.isEmpty &&
      (freeBuffers.size >= freeSlots || freeBytes + arriving > windowBytes)
    ) {
      freeBytes -= freeBuffers.removeLast().length
      dropOldestUntilRoom(arriving)
    }

  private def takeDeflater(): Deflater =
    Option(freeDeflaters.pollFirst()).fold(newDeflater(level)) { recycled =>
      recycled.reset()
      recycled
    }

  /**
   * Handed back only at the level it was made with: `setLevel` defers to zlib's `deflateParams`,
   * which changes the output at every level when input arrives in chunks.
   */
  private def giveDeflater(d: Deflater, atLevel: Int): Unit =
    if (atLevel == level && freeDeflaters.size < recycled) freeDeflaters.addFirst(d)
    else d.end()

  private def endFreeDeflaters(): Unit = {
    freeDeflaters.forEach(_.end())
    freeDeflaters.clear()
  }

  // ── record writers ──────────────────────────────────────────────────

  private def writeLOC(x: XEntry): Unit = {
    val useDescriptor = (x.flag & DescriptorFlag) != 0
    val hasZip64 = !useDescriptor && (x.csize >= Zip64Magic || x.size >= Zip64Magic)
    val version =
      if (hasZip64) VersionZip64
      else if (x.method == ZipEntry.DEFLATED) VersionDeflated
      else VersionStored
    val elen = x.reference.local.length + (if (hasZip64) Zip64LocalFieldBytes else 0)
    writeInt(LocSig); writeShort(version); writeShort(x.flag); writeShort(x.method)
    writeInt(x.reference.dosTime)
    if (useDescriptor) {
      writeInt(0L); writeInt(0L); writeInt(0L)
    } else if (hasZip64) {
      writeInt(x.crc); writeInt(Zip64Magic); writeInt(Zip64Magic)
    } else {
      writeInt(x.crc); writeInt(x.csize); writeInt(x.size)
    }
    writeShort(x.name.length); writeShort(elen)
    writeBytes(x.name)
    if (hasZip64) {
      writeShort(Zip64ExtraId); writeShort(Zip64LocalDataBytes)
      writeLong(x.size); writeLong(x.csize)
    }
    writeBytes(x.reference.local)
    locoff = written
  }

  private def writeEXT(x: XEntry): Unit = {
    writeInt(ExtSig); writeInt(x.crc)
    if (x.csize >= Zip64Magic || x.size >= Zip64Magic) {
      writeLong(x.csize); writeLong(x.size)
    } else {
      writeInt(x.csize); writeInt(x.size)
    }
  }

  private def writeCEN(x: XEntry): Unit = {
    val zip64Size = x.size >= Zip64Magic
    val zip64Compressed = x.csize >= Zip64Magic
    val zip64Offset = x.offset >= Zip64Magic
    val moved = (if (zip64Size) Zip64FieldBytes else 0) +
      (if (zip64Compressed) Zip64FieldBytes else 0) +
      (if (zip64Offset) Zip64FieldBytes else 0)
    val added = if (moved > 0) ExtraHeaderBytes + moved else 0
    val extraLength = x.reference.central.length + added
    // the reference's own verdict rather than its length weighed again: it weighs the entry's extra
    // field and not the longer one it writes. The zip64 field its probe could not carry is asked
    // apart. A JDK with no refusal to give writes an oversized header anyway, and so does this
    if (x.reference.refusesCentral || added >= x.reference.centralSlack)
      ZipReference.oversizedCentralHeaderRefusal.foreach(refusal => throw new ZipException(refusal))
    // cut to what its length field holds, where the reference cuts it — a name past the field is
    // written whole under a wrapped length instead. The refusal above still weighs the whole comment
    val commentLength = math.min(x.comment.length, MaxFieldBytes)
    val version =
      if (moved > 0) VersionZip64
      else if (x.method == ZipEntry.STORED) VersionStored
      else VersionDeflated
    // only the platform half carries over: the rest is the version the probe reported for a stored entry
    val madeBy = (x.reference.madeBy & PlatformMask) | version
    writeInt(CenSig); writeShort(madeBy); writeShort(version)
    writeShort(x.flag); writeShort(x.method)
    writeInt(x.reference.dosTime); writeInt(x.crc)
    writeInt(if (zip64Compressed) Zip64Magic else x.csize)
    writeInt(if (zip64Size) Zip64Magic else x.size)
    writeShort(x.name.length)
    writeShort(extraLength)
    writeShort(commentLength)
    writeShort(0); writeShort(0); writeInt(x.reference.attributes)
    writeInt(if (zip64Offset) Zip64Magic else x.offset)
    writeBytes(x.name)
    if (moved > 0) {
      writeShort(Zip64ExtraId); writeShort(moved)
      if (zip64Size) writeLong(x.size)
      if (zip64Compressed) writeLong(x.csize)
      if (zip64Offset) writeLong(x.offset)
    }
    writeBytes(x.reference.central); writeBytes(x.comment, 0, commentLength)
  }

  private def writeEND(directoryOffset: Long, directorySize: Long): Unit = {
    val count = xentries.size
    // the count is the one trigger the reference lets a caller turn off; sizes and offsets are not
    val tooMany = count >= MaxEntriesWithoutZip64 && !InhibitZip64
    val hasZip64 = tooMany || directorySize >= Zip64Magic || directoryOffset >= Zip64Magic
    if (hasZip64) {
      val at = written
      writeInt(Zip64EndSig); writeLong(Zip64EndTrailingBytes)
      writeShort(VersionZip64); writeShort(VersionZip64)
      writeInt(0L); writeInt(0L)
      writeLong(count.toLong); writeLong(count.toLong)
      writeLong(directorySize); writeLong(directoryOffset)
      writeInt(Zip64LocatorSig); writeInt(0L); writeLong(at); writeInt(OneDisk)
    }
    val counted = if (hasZip64) math.min(count, MaxEntriesWithoutZip64) else count
    writeInt(EndSig); writeShort(0); writeShort(0); writeShort(counted); writeShort(counted)
    writeInt(math.min(directorySize, Zip64Magic)); writeInt(math.min(directoryOffset, Zip64Magic))
    writeShort(comment.length); writeBytes(comment)
  }

  // ── binary primitives ────────────────────────────────────────────────

  private def claim(name: String): Unit =
    if (!names.add(name)) throw new ZipException("duplicate entry: " + name)

  private def writeShort(v: Int): Unit = {
    out.write(v & 0xff); out.write((v >>> 8) & 0xff)
    written += 2
  }

  private def writeInt(v: Long): Unit = {
    out.write((v & 0xff).toInt); out.write(((v >>> 8) & 0xff).toInt)
    out.write(((v >>> 16) & 0xff).toInt); out.write(((v >>> 24) & 0xff).toInt)
    written += 4
  }

  private def writeLong(v: Long): Unit = { writeInt(v); writeInt(v >>> 32) }

  private def writeBytes(b: Array[Byte]): Unit = writeBytes(b, 0, b.length)

  private def writeBytes(b: Array[Byte], off: Int, len: Int): Unit = {
    out.write(b, off, len)
    written += len
  }
}

private[sbt] object ParallelZipOutputStream {

  // ── defaults ─────────────────────────────────────────────────────────

  /**
   * How much buffered input may be in flight per writer. Fixed rather than a share of the heap, so
   * an archive that fits on one machine fits on another.
   */
  final val WindowBytes: Long = 16L * 1024 * 1024

  /**
   * Past this an entry is streamed rather than held; the bytes are identical either way. The whole
   * window, since an entry larger than that cannot share the pipeline with anything else.
   */
  final val MaxEntryBytes: Long = WindowBytes

  // ── buffer and deflater tuning ───────────────────────────────────────

  private final val SinkBufferBytes = 64 * 1024
  private final val DeflateBlockBytes = 64 * 1024
  private final val MinBufferBytes = 4096

  private final val InitialWrites = 64

  /** Under this much a write, the list of where they ended outweighs what they wrote. */
  private final val MinBytesPerHeldWrite = 16

  /**
   * How many write boundaries an entry may have before it streams instead. Declared below its
   * divisor: read above it, a divisor no longer folded to a literal would still be zero here.
   */
  private final val MaxHeldWrites = (WindowBytes / MinBytesPerHeldWrite).toInt

  /**
   * What a held entry grows by. Big enough that a class-sized entry is one of them and the list of
   * them stays short, small enough that the last one is mostly used rather than mostly slack.
   */
  private final val HoldBlockBytes = 64 * 1024

  private final val InitialBlocks = 16
  private final val MinGrowthBytes = 64

  /** An entry in flight holds two buffers: the one it was written into, and the one it deflates to. */
  private final val BuffersPerEntry = 2

  /** Room over those for what an entry that outgrew its first buffer left behind on the way up. */
  private final val SpareBuffers = 8

  /** A ceiling on what is kept, since `parallelism` comes from a build setting and can be anything. */
  private final val MaxRecycled = 64

  /**
   * How much wider than it would have allocated a recycled buffer may be: the in-flight window
   * counts what a buffer measures, not what it carries.
   */
  private final val MaxFitFactor = 2

  private final val DeflateOverheadDivisor = 1000
  private final val MinDeflateOverheadBytes = 128

  /**
   * The reference's escape hatch for readers that cannot handle a zip64 end record, which caps an
   * archive at what the 16 bit entry count holds. Read once, as it reads it.
   */
  private val InhibitZip64: Boolean = java.lang.Boolean.getBoolean("jdk.util.zip.inhibitZip64")

  /** The high byte of "version made by": the platform, which is all that carries from the probe. */
  private final val PlatformMask = 0xff00

  /** No level a caller can set, marking a deflater whose level moved under it as unpoolable. */
  private final val DirtyLevel = -2

  private def deflatedBound(size: Long): Long =
    size + size / DeflateOverheadDivisor + MinDeflateOverheadBytes

  /**
   * Rounded so entries of similar size take each other's buffers: to a power of two while that is
   * cheap, and to whole blocks once it is not.
   */
  private def rounded(atLeast: Int): Int =
    if (atLeast <= MinBufferBytes) MinBufferBytes
    else if (atLeast <= HoldBlockBytes) Integer.highestOneBit(atLeast - 1) << 1
    else if (atLeast > Int.MaxValue - HoldBlockBytes) atLeast
    else (atLeast + HoldBlockBytes - 1) / HoldBlockBytes * HoldBlockBytes

  // ── data classes ─────────────────────────────────────────────────────

  /**
   * One entry's record, as `ZipOutputStream.XEntry` is. Immutable: what writing settles comes back
   * as a new record rather than being written into one an entry in flight belongs to.
   */
  private final class XEntry(
      val name: Array[Byte],
      val comment: Array[Byte],
      val reference: ZipReference.Header,
      val method: Int,
      val flag: Int,
      val crc: Long,
      val csize: Long,
      val size: Long,
      val offset: Long
  ) {

    def at(offset: Long): XEntry = placed(offset, crc, csize, size)

    def settled(crc: Long, csize: Long, size: Long): XEntry = placed(offset, crc, csize, size)

    def placed(offset: Long, crc: Long, csize: Long, size: Long): XEntry =
      new XEntry(name, comment, reference, method, flag, crc, csize, size, offset)
  }

  private final class Deflated(
      val bytes: Array[Byte],
      val crc: Long,
      val size: Long,
      val csize: Long
  )

  /** [[DirtyLevel]] once `setLevel` has moved the deflater, since no other entry may reuse it. */
  private final class Deflating(val deflater: Deflater, val buffer: Array[Byte], val level: Int) {

    def levelChanged(buffer: Array[Byte]): Deflating = new Deflating(deflater, buffer, DirtyLevel)
  }

  /** An entry being deflated elsewhere, with everything its append owes back to the free lists. */
  private final class Pending(
      val entry: XEntry,
      task: FutureTask[Deflated],
      val input: Array[Array[Byte]],
      val capacity: Long,
      val deflater: Deflater,
      val level: Int
  ) {

    /** The deflation, run here if the context has not started it — `run` is a no-op once it has. */
    def claimed(): FutureTask[Deflated] = {
      task.run()
      task
    }

    /**
     * Ends the deflater for an entry `close` will never append, once the deflation is over — run
     * here if nothing else ran it. Ending one part way through a `deflate` would leave the call
     * after it on a closed deflater, so an interrupt does not cut the wait short.
     */
    def endDeflater(): Unit = {
      if (settled(false)) Thread.currentThread().interrupt()
      deflater.end()
    }

    /** The deflation's outcome is discarded; whether waiting for it was interrupted is not. */
    @tailrec private def settled(interrupted: Boolean): Boolean = {
      val cut =
        try { val _ = claimed().get(); false }
        catch {
          case _: InterruptedException => true
          case _: Throwable            => false
        }
      if (cut) settled(true) else interrupted
    }
  }

  /**
   * The entry being written, in blocks rather than one array grown into: doubling costs 2N allocated
   * and N copied. Blocks leave only through [[take]], so no field here is reached by two threads.
   */
  private final class HeldEntry(newBlock: Int => Array[Byte]) {
    private var blocks = new Array[Array[Byte]](InitialBlocks)
    private var used = new Array[Int](InitialBlocks)
    private var count = 0
    private var heldBytes = 0

    // where each write ended: what zlib leaves pending depends on it, and a mid-entry `setLevel`
    // flushes a block from whatever is pending — so replaying a held entry repeats the caller's calls
    private var writeLengths = new Array[Int](InitialWrites)
    private var writeCount = 0

    /** An open entry has its first block already, so nothing else can be holding none. */
    def isOpen: Boolean = count > 0
    def bytes: Int = heldBytes
    def writes: Int = writeCount

    def open(): Unit = {
      clear()
      addBlock(HoldBlockBytes)
    }

    def put(b: Array[Byte], off: Int, len: Int): Unit = {
      // never split across two blocks: half a call in each of two is not one call to replay
      val room = blocks(count - 1).length - used(count - 1)
      if (len > room) addBlock(len)
      val at = count - 1
      Array.copy(b, off, blocks(at), used(at), len)
      used(at) += len
      heldBytes += len
      recordWrite(len)
    }

    def take(): Taken = {
      val out = Arrays.copyOf(blocks, count)
      val taken = new Taken(
        blocks = out,
        used = Arrays.copyOf(used, count),
        writes = Arrays.copyOf(writeLengths, writeCount),
        bytes = heldBytes,
        capacity = out.foldLeft(0L)(_ + _.length)
      )
      // emptied rather than left behind, so a slot past the count keeps no buffer alive
      (0 until count).foreach(blocks(_) = null)
      clear()
      taken
    }

    private def clear(): Unit = {
      count = 0
      heldBytes = 0
      writeCount = 0
    }

    private def addBlock(atLeast: Int): Unit = {
      if (count == blocks.length) {
        blocks = Arrays.copyOf(blocks, blocks.length * 2)
        used = Arrays.copyOf(used, used.length * 2)
      }
      blocks(count) = newBlock(math.max(atLeast, HoldBlockBytes))
      used(count) = 0
      count += 1
    }

    private def recordWrite(len: Int): Unit = {
      if (writeCount == writeLengths.length)
        writeLengths = Arrays.copyOf(writeLengths, writeLengths.length * 2)
      writeLengths(writeCount) = len
      writeCount += 1
    }
  }

  /** What [[HeldEntry.take]] hands over, which the caller owns until it gives the blocks back. */
  private final class Taken(
      val blocks: Array[Array[Byte]],
      val used: Array[Int],
      val writes: Array[Int],
      val bytes: Int,
      val capacity: Long
  ) {

    /** The caller's own writes back, in the calls they arrived in rather than in blocks of ours. */
    def replayTo(to: (Array[Byte], Int, Int) => Unit): Unit = {
      // where a write sits follows from the lengths, since blocks fill in order and none spans two
      @tailrec def next(i: Int, block: Int, at: Int): Unit =
        if (i < writes.length) {
          val len = writes(i)
          val onto = if (at + len > used(block)) block + 1 else block
          val from = if (onto == block) at else 0
          to(blocks(onto), from, len)
          next(i + 1, onto, from + len)
        }
      next(0, 0, 0)
    }
  }

  // ── deflation ────────────────────────────────────────────────────────

  private final class Filled(val buffer: Array[Byte], val upTo: Int)

  /** Reads only the blocks it was handed, so what comes back is the whole of the result. */
  private def deflateInto(
      deflater: Deflater,
      data: Array[Array[Byte]],
      used: Array[Int],
      len: Int,
      into: Array[Byte],
      window: Int
  ): Deflated = {
    val sum = new CRC32
    @tailrec def drain(done: () => Boolean, filled: Filled): Filled =
      if (done()) filled
      else {
        val room = if (filled.upTo < filled.buffer.length) filled.buffer else grown(filled.buffer)
        val wrote = deflater.deflate(room, filled.upTo, math.min(window, room.length - filled.upTo))
        drain(done, new Filled(room, filled.upTo + wrote))
      }
    // block by block, which the entry is in anyway: this deflater sees the whole entry and finishes
    // it, so only the room its output is given frames what it emits
    val deflated = data.indices.foldLeft(new Filled(into, 0)) { (filled, b) =>
      sum.update(data(b), 0, used(b))
      deflater.setInput(data(b), 0, used(b))
      drain(() => deflater.needsInput(), filled)
    }
    deflater.finish()
    val whole = drain(() => deflater.finished(), deflated)
    new Deflated(whole.buffer, sum.getValue, len.toLong, whole.upTo.toLong)
  }

  /** Half as much again, so that a bound that fell short is not met one deflate at a time. */
  private def grown(buffer: Array[Byte]): Array[Byte] =
    Arrays.copyOf(buffer, buffer.length + (buffer.length >> 1) + MinGrowthBytes)

  private def await(task: FutureTask[Deflated]): Deflated =
    try task.get()
    catch {
      case e: ExecutionException => throw Option(e.getCause).getOrElse(e)
      // `get` throws a checked exception no `OutputStream` caller can catch, and clears the flag on
      // its way out. Both are put back: sbt cancels a task by interrupting it
      case interrupted: InterruptedException =>
        Thread.currentThread().interrupt()
        throw new InterruptedIOException("interrupted while deflating").initCause(interrupted)
    }

  // ── names and comments ───────────────────────────────────────────────

  private def utf8(name: String): Array[Byte] = name.getBytes(StandardCharsets.UTF_8)

  private def commentBytes(e: ZipEntry): Array[Byte] =
    Option(e.getComment).fold(Array.emptyByteArray)(utf8)
}
