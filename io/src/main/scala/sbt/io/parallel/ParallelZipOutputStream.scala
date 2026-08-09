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
import java.util.concurrent.{ CompletableFuture, ExecutionException, Executor, ForkJoinPool }
import java.util.zip.{ CRC32, Deflater, ZipEntry, ZipException }
import scala.collection.mutable.{ HashSet, ListBuffer }
import scala.concurrent.ExecutionContext
import ZipConstants._

/**
 * A zip writer that deflates entries in parallel while producing byte-for-byte identical output to
 * `ZipOutputStream`, whose shape it keeps: the same `written` and `locoff` counters, the same
 * `current` entry, and the same `writeLOC`, `writeEXT`, `writeCEN` and `writeEND` records over the
 * same `XEntry`.
 *
 * Where JDK versions disagree, the answer is read from a reference writer at run time rather than
 * restated here, so a new JDK does not need this file changed. [[ZipReference]] is every one of
 * those questions and nothing else. One refusal is this writer's own: an entry opened after
 * `finish`, which the reference appends past the end record.
 *
 * An entry is held in a buffer, deflated on the caller's execution context and appended in order.
 * One that is stored, declares all three sizes, is written at level 0, or outgrows [[maxEntryBytes]]
 * is streamed straight through instead: its sizes belong in its own header, its blocks are framed
 * from the caller's own writes, or it is too large to hold.
 *
 * Buffers and deflaters are recycled on the writing thread, so neither is allocated per entry once
 * an archive is under way.
 *
 * Not thread safe. Names and comments are always UTF-8, as `IO.defaultCharset` is.
 *
 * @param parallelism how many entries may be in flight, not a thread count. Each in-flight entry
 *                    holds a `Deflater`, whose zlib state is around 256 KB of native memory that
 *                    [[windowBytes]] does not account for, and up to 64 more are kept for reuse
 * @param ec where deflation runs. The writing thread waits on it once [[parallelism]] entries are in
 *           flight, so a context served only by that thread has nothing left to make progress with,
 *           and one that drops work without throwing leaves that wait with nothing to end it
 */
private[sbt] class ParallelZipOutputStream(
    to: OutputStream,
    parallelism: Int = ParallelZipOutputStream.DefaultParallelism
)(implicit ec: ExecutionContext)
    extends ZipSink {
  import ParallelZipOutputStream._

  require(parallelism >= 1, "parallelism must be at least 1, was " + parallelism)

  private val out = new BufferedOutputStream(to, SinkBufferBytes)
  private var written = 0L
  private var locoff = 0L

  private val xentries = ListBuffer.empty[XEntry]
  private val names = HashSet.empty[String]

  private var current: XEntry = null
  private var finished = false

  private var level = Deflater.DEFAULT_COMPRESSION
  private var method = ZipEntry.DEFLATED
  private var comment: Array[Byte] = Array.emptyByteArray

  // held as blocks rather than one array grown into: doubling costs 2N allocated and N copied
  private var blocks = new Array[Array[Byte]](InitialBlocks)
  private var blockUsed = new Array[Int](InitialBlocks)
  private var blockCount = 0
  private var buffered = 0
  private var holding = false

  /** [[maxEntryBytes]], capped at what an `Int` of held bytes holds. Past it an entry is streamed. */
  private def holdCeiling: Long = math.min(maxEntryBytes, Int.MaxValue.toLong)

  // where each write ended: what zlib leaves pending depends on it, and a mid-entry `setLevel`
  // flushes a block from whatever is pending — so replaying a held entry repeats the caller's calls
  private var writeLengths = new Array[Int](InitialWrites)
  private var writeCount = 0

  private val oneByte = new Array[Byte](1)

  // one sink for every question, since a reference is asked one per entry
  private val probeSink = new ZipReference.ProbeSink

  private val inFlight = new ArrayDeque[Pending]
  private var inFlightBytes = 0L

  // `CompletableFuture` hands work to an `Executor`, which an `ExecutionContext` is not
  private val executor: Executor = (r: Runnable) => ec.execute(r)

  // streaming state — set while an entry is written straight through instead of held
  private var streaming = false
  private val crc = new CRC32
  private var deflater: Deflater = null
  private var deflaterLevel = 0
  private var deflateBuf: Array[Byte] = null

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

  /**
   * How much held input may be in flight at once, and how large an entry may be before it is
   * streamed instead of held. Fixed rather than offered to a caller, as `DeflaterOutputStream` fixes
   * the buffer it deflates through. Overridable only so a test can drive the streaming path without
   * an archive large enough to reach it: the bytes are the same either way.
   */
  protected def windowBytes: Long = WindowBytes

  protected def maxEntryBytes: Long = MaxEntryBytes

  /**
   * Whether to deflate where the entry was written rather than handing it to the context. A fact
   * about `commonPool`, so asked only where `commonPool` is what the work would go to: with no spare
   * core the handover gains nothing, and on 8 a common pool configured with no parallelism at all
   * never runs what it is given, so handing an entry over there would not be slow but stuck. A
   * context the caller supplied is used as given. Overridable so a test can drive either path.
   */
  protected def deflateOnThisThread: Boolean =
    (ec eq ParallelZipOutputStream.commonPoolContext) &&
      ForkJoinPool.getCommonPoolParallelism <= 1

  // ── entry lifecycle ──────────────────────────────────────────────────

  def putNextEntry(e: ZipEntry): Unit = {
    if (finished) throw new ZipException("the archive is finished")
    // before the entry is looked at, as `JarOutputStream` stamps it: one refused below has still
    // spent the jar's one magic field
    stamp(e)
    if (current != null) closeEntry()
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
    current = new XEntry(
      name = utf8(e.getName),
      comment = commentBytes(e),
      reference = reference,
      method = entryMethod,
      flag = Utf8Flag | (if (sizesInHeader) 0 else DescriptorFlag),
      crc = e.getCrc,
      csize = e.getCompressedSize,
      size = e.getSize
    )
    // level 0 streams: zlib frames a stored block from what it has been handed, so only passing the
    // caller's writes through unchanged reproduces the reference's block boundaries
    val uncompressed = entryMethod == ZipEntry.DEFLATED && level == Deflater.NO_COMPRESSION
    if (sizesInHeader || uncompressed || e.getSize > maxEntryBytes) startStreaming()
    else {
      // a block at a time whatever the entry declares, so any block recycles for any entry
      holding = true
      blockCount = 0
      buffered = 0
      writeCount = 0
      addBlock(HoldBlockBytes)
    }
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
    else if (!holding) throw new ZipException("no current ZIP entry")
    else if (buffered.toLong + len > holdCeiling || writeCount == MaxHeldWrites) {
      // decided before the copy, or the peak this bounds would be whatever arrived in one array.
      // What was held reaches the deflater as its own call, which is safe only because a held entry
      // is never at level 0: levels 1 to 9 frame blocks from input, not from calls
      startStreaming()
      writeStreaming(b, off, len)
    } else {
      // never split across two blocks: half a call in each of two is not one call to replay
      val room = blocks(blockCount - 1).length - blockUsed(blockCount - 1)
      if (len > room) addBlock(len)
      val at = blockCount - 1
      System.arraycopy(b, off, blocks(at), blockUsed(at), len)
      blockUsed(at) += len
      buffered += len
      recordWrite(len)
    }
  }

  /** A block for at least `atLeast` bytes, which is a whole write's worth or the usual size. */
  private def addBlock(atLeast: Int): Unit = {
    if (blockCount == blocks.length) {
      blocks = Arrays.copyOf(blocks, blocks.length * 2)
      blockUsed = Arrays.copyOf(blockUsed, blockUsed.length * 2)
    }
    blocks(blockCount) = takeBuffer(math.max(atLeast, HoldBlockBytes))
    blockUsed(blockCount) = 0
    blockCount += 1
  }

  def closeEntry(): Unit =
    if (current != null) {
      if (streaming) {
        // cleared only once the entry is accepted, as the reference clears it: one it refused stays
        // open, so a caller carrying on is refused against what it has written since
        closeStreaming(current)
        streaming = false
        current = null
      } else if (holding) {
        val x = current
        current = null
        submit(x)
      }
    }

  def finish(): Unit =
    if (!finished) {
      if (current != null) closeEntry()
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
        if (deflater != null) {
          deflater.end()
          deflater = null
          deflateBuf = null
        }
        // ended by the thread deflating with it: cancelling would complete the future while that
        // deflation was still running, and end the deflater under it
        while (!inFlight.isEmpty) {
          val pending = inFlight.removeFirst()
          val _ = pending.future.whenComplete((_, _) => pending.deflater.end())
        }
        inFlightBytes = 0L
        while (!freeDeflaters.isEmpty) freeDeflaters.removeFirst().end()
        freeBuffers.clear()
        freeBytes = 0L
      } finally out.close()

  // ── settings ─────────────────────────────────────────────────────────

  def setLevel(level: Int): Unit = {
    ZipReference.validate(_.setLevel(level))
    if (level != this.level) {
      // a held entry has no deflater for `setLevel` to reach, so it streams first: what it holds is
      // replayed at the level it was held at, leaving the same bytes deflated as the reference's
      if (holding) startStreaming()
      if (deflater != null) {
        deflater.setLevel(level)
        // zlib settles the change by flushing a block, and the room given for that reaches the
        // output at every level: measured, 64 KB diverges where the reference's own 512 matches
        deflateBuf = new Array[Byte](ZipReference.deflateBuffer)
        deflaterLevel = DirtyLevel
      }
      // made at the level being left behind, so none will deflate like one made at this one
      while (!freeDeflaters.isEmpty) freeDeflaters.removeFirst().end()
      this.level = level
    }
  }

  def setMethod(method: Int): Unit = {
    ZipReference.validate(_.setMethod(method))
    this.method = method
  }

  def setComment(comment: String): Unit =
    if (comment != null) {
      ZipReference.validate(_.setComment(comment))
      this.comment = utf8(comment)
    } else if (ZipReference.nullCommentClears) this.comment = Array.emptyByteArray

  // ── streaming path ──────────────────────────────────────────────────

  /**
   * Begins streaming the open entry. Everything queued is appended first, since this entry's header
   * goes down at the current offset, and whatever was buffered for it leads its content.
   */
  private def startStreaming(): Unit = {
    drainAll()
    val x = current
    val heldBlocks = blockCount
    val heldWrites = writeCount
    holding = false
    blockCount = 0
    buffered = 0
    writeCount = 0
    x.offset = written
    writeLOC(x)
    streaming = true
    crc.reset()
    if (x.method == ZipEntry.DEFLATED) {
      deflaterLevel = level
      deflater = takeDeflater()
      // the reference's own buffer: a `deflate` given more room drains more of zlib's pending
      // output, which is what a later `setLevel` on this entry would flush a block from
      deflateBuf = new Array[Byte](ZipReference.deflateBuffer)
    }
    // in the caller's own writes rather than in blocks of ours — see [[writeLengths]]. Where each
    // sits follows from the lengths, since blocks fill in order and no write spans two
    var block = 0
    var at = 0
    var i = 0
    while (i < heldWrites) {
      val len = writeLengths(i)
      if (at + len > blockUsed(block)) {
        block += 1
        at = 0
      }
      writeStreaming(blocks(block), at, len)
      at += len
      i += 1
    }
    var b = 0
    while (b < heldBlocks) {
      giveBuffer(blocks(b))
      blocks(b) = null
      b += 1
    }
  }

  private def recordWrite(len: Int): Unit = {
    if (writeCount == writeLengths.length)
      writeLengths = Arrays.copyOf(writeLengths, writeLengths.length * 2)
    writeLengths(writeCount) = len
    writeCount += 1
  }

  private def writeStreaming(b: Array[Byte], off: Int, len: Int): Unit = {
    if (deflater != null) {
      // an entry the reference refused keeps the deflater it finished, which takes no more input:
      // `deflate` would return nothing while `needsInput` stayed false and the loop would not end
      if (deflater.finished()) throw new IOException("write beyond end of stream")
      deflater.setInput(b, off, len)
      while (!deflater.needsInput()) {
        val n = deflater.deflate(deflateBuf, 0, deflateBuf.length)
        if (n > 0) writeBytes(deflateBuf, 0, n)
      }
    } else {
      // counted before it is refused, as the reference counts it
      written += len
      if (written - locoff > current.size)
        throw new ZipException("attempt to write past end of STORED entry")
      out.write(b, off, len)
    }
    crc.update(b, off, len)
  }

  private def closeStreaming(x: XEntry): Unit = {
    if (x.method == ZipEntry.DEFLATED) {
      deflater.finish()
      while (!deflater.finished()) {
        val n = deflater.deflate(deflateBuf, 0, deflateBuf.length)
        if (n > 0) writeBytes(deflateBuf, 0, n)
      }
      val size = deflater.getBytesRead
      val csize = deflater.getBytesWritten
      if ((x.flag & DescriptorFlag) == 0) {
        if (x.size != size) throw wrongSize("size", x.size, size)
        if (x.csize != csize) throw wrongSize("compressed size", x.csize, csize)
        if (x.crc != crc.getValue) throw wrongCrc("CRC-32", x.crc, crc.getValue)
      } else {
        x.size = size
        x.csize = csize
        x.crc = crc.getValue
        writeEXT(x)
      }
      // handed back only once the entry is accepted, so closing a refused one re-runs the check
      giveDeflater(deflater, deflaterLevel)
      deflater = null
      deflateBuf = null
    } else {
      val size = written - locoff
      if (x.size != size) throw wrongSize("size", x.size, size)
      // the reference spells the label in lower case on this path and in upper on the other
      if (x.crc != crc.getValue) throw wrongCrc("crc-32", x.crc, crc.getValue)
    }
    val _ = xentries += x
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
    val count = blockCount
    val data = Arrays.copyOf(blocks, count)
    val used = Arrays.copyOf(blockUsed, count)
    val len = buffered
    var held = 0L
    var i = 0
    while (i < count) {
      held += data(i).length
      blocks(i) = null
      i += 1
    }
    holding = false
    blockCount = 0
    buffered = 0
    writeCount = 0
    // drained before anything is taken, so this entry gets what the appended one just handed back
    while (
      !inFlight.isEmpty &&
      (inFlight.size >= parallelism || inFlightBytes + held > windowBytes)
    ) drainOne()
    // before the deflater, so a heap that cannot take it fails with no deflater to strand
    val bound = math.min(deflatedBound(len.toLong), Int.MaxValue.toLong).toInt
    val into = takeBuffer(bound)
    // still the level this entry was opened at: `setLevel` moves a held entry to the streaming path
    val entryLevel = level
    val entryDeflater = takeDeflater()
    // a window wider than the reference's is safe here and nowhere else: this entry is deflated
    // whole and on its own, so no level change lands part way through it
    val window = DeflateBlockBytes
    def deflate(): Array[Byte] = deflateInto(x, entryDeflater, data, used, len, into, window)
    if (deflateOnThisThread) {
      // ended here if it fails: this branch never queues, and `close` only sweeps the queue
      val deflated =
        try deflate()
        catch {
          case failed: Throwable =>
            entryDeflater.end()
            throw failed
        }
      giveBlocks(data)
      giveDeflater(entryDeflater, entryLevel)
      append(x, deflated)
      giveBuffer(deflated)
    } else {
      // a refused submission never reaches the queue either, so its deflater is ended here too
      val future =
        try CompletableFuture.supplyAsync(() => deflate(), executor)
        catch {
          case failed: Throwable =>
            entryDeflater.end()
            throw failed
        }
      val _ = inFlight.addLast(new Pending(x, future, data, held, entryDeflater, entryLevel))
      inFlightBytes += held
    }
  }

  private def drainOne(): Unit = {
    // left queued until its result is in hand, so a failed deflation still has `close` to end it
    val p = inFlight.getFirst
    val deflated = await(p.future)
    val _ = inFlight.removeFirst()
    inFlightBytes -= p.bytes
    giveBlocks(p.input)
    giveDeflater(p.deflater, p.level)
    append(p.entry, deflated)
    giveBuffer(deflated)
  }

  private def drainAll(): Unit = while (!inFlight.isEmpty) drainOne()

  private def giveBlocks(taken: Array[Array[Byte]]): Unit = {
    var i = 0
    while (i < taken.length) {
      giveBuffer(taken(i))
      i += 1
    }
  }

  private def append(x: XEntry, deflated: Array[Byte]): Unit = {
    x.offset = written
    writeLOC(x)
    writeBytes(deflated, 0, x.csize.toInt)
    writeEXT(x)
    val _ = xentries += x
  }

  // ── recycling ────────────────────────────────────────────────────────

  /**
   * A buffer at least this long from what an earlier entry handed back, but not one so much wider
   * that allocating would have been better: an entry is charged against the in-flight window for
   * what its blocks measure, so a multi-megabyte buffer taken as a 64 KB block is charged megabytes
   * for kilobytes. Only decides anything where [[windowBytes]] binds before `parallelism` does,
   * which takes more cores than a laptop has — measured on 12, the bound moves nothing.
   */
  private def takeBuffer(atLeast: Int): Array[Byte] = {
    val most = MaxFitFactor.toLong * rounded(atLeast)
    var found: Array[Byte] = null
    val each = freeBuffers.iterator
    while (found == null && each.hasNext) {
      val b = each.next()
      if (b.length >= atLeast && b.length <= most) {
        each.remove()
        found = b
      }
    }
    if (found == null) new Array[Byte](rounded(atLeast))
    else {
      freeBytes -= found.length
      found
    }
  }

  /**
   * Kept within the bytes the window already bounds, and to no more buffers than are worth scanning.
   * The oldest goes to make room: turning the arriving one away would freeze the list as it stands.
   */
  private def giveBuffer(b: Array[Byte]): Unit = {
    while (
      !freeBuffers.isEmpty &&
      (freeBuffers.size >= freeSlots || freeBytes + b.length > windowBytes)
    ) freeBytes -= freeBuffers.removeLast().length
    if (freeBytes + b.length <= windowBytes) {
      freeBuffers.addFirst(b)
      freeBytes += b.length
    }
  }

  private def takeDeflater(): Deflater =
    freeDeflaters.pollFirst() match {
      case null => new Deflater(level, true)
      case d    =>
        d.reset()
        d
    }

  /**
   * Handed back only at the level it was made with: `setLevel` defers to zlib's `deflateParams`,
   * which changes the output at every level when input arrives in chunks.
   */
  private def giveDeflater(d: Deflater, atLevel: Int): Unit =
    if (atLevel == level && freeDeflaters.size < recycled) freeDeflaters.addFirst(d)
    else d.end()

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
    // field and not the longer one it writes. The zip64 field its probe could not carry is asked apart
    if (x.reference.refusesCentral || added >= x.reference.centralSlack)
      throw new ZipException(ZipReference.oversizedCentralHeaderRefusal)
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
   * How much buffered input may be in flight per writer. Bounds retention across entries, not within
   * one: a single entry is held whole up to [[MaxEntryBytes]]. A fixed size rather than a share of the
   * heap, as `DeflaterOutputStream` fixes the 512 bytes it deflates through, so that an archive costs
   * the same to write wherever it is written and one that fits on one machine fits on another.
   */
  final val WindowBytes: Long = 16L * 1024 * 1024

  /**
   * Past this an entry is streamed rather than held, however it arrives — in pieces or as a single
   * array. Held and streamed entries produce identical bytes, so the threshold changes only cost.
   *
   * The whole in-flight window, because an entry larger than that cannot share the pipeline with
   * anything already in it: [[ParallelZipOutputStream.submit]] drains until the window has room, and
   * for an entry this size that means draining it to nothing.
   */
  final val MaxEntryBytes: Long = WindowBytes

  final val DefaultParallelism: Int = math.max(1, Runtime.getRuntime.availableProcessors)

  /**
   * `commonPool` as a context, which is what `CompletableFuture` hands to. Named here so that
   * [[ParallelZipOutputStream.deflateOnThisThread]] can recognise it, since its fallback to a thread
   * per task below two cores is a fact about this pool and no other. Nothing defaults to it:
   * `IO.Implicits.deflateContext` is where it is offered to callers.
   */
  val commonPoolContext: ExecutionContext =
    ExecutionContext.fromExecutor(ForkJoinPool.commonPool())

  // ── buffer and deflater tuning ───────────────────────────────────────

  private final val SinkBufferBytes = 64 * 1024
  private final val DeflateBlockBytes = 64 * 1024
  private final val MinBufferBytes = 4096

  /** Room for a held entry's write boundaries before the list has to grow. */
  private final val InitialWrites = 64

  /** Under this much a write, the list of where they ended outweighs what they wrote. */
  private final val MinBytesPerHeldWrite = 16

  /**
   * How many of those boundaries an entry may have before it is streamed rather than held. Four
   * bytes to remember a write is nothing against a large one and four times what a one-byte write
   * holds. Past this it streams, where boundaries stop needing remembering.
   *
   * Declared below its divisor rather than above it: a divisor that stopped being folded to a
   * literal — a type ascription is enough — would be read here before it was assigned, dividing by
   * zero as the object loaded.
   */
  private final val MaxHeldWrites = (WindowBytes / MinBytesPerHeldWrite).toInt

  /**
   * What a held entry grows by. Big enough that a class-sized entry is one of them and the list of
   * them stays short, small enough that the last one is mostly used rather than mostly slack.
   */
  private final val HoldBlockBytes = 64 * 1024

  /** Blocks an entry can hold before the list of them has to grow: 1 MB at [[HoldBlockBytes]]. */
  private final val InitialBlocks = 16
  private final val MinGrowthBytes = 64

  /** An entry in flight holds two buffers: the one it was written into, and the one it deflates to. */
  private final val BuffersPerEntry = 2

  /** Room over those for what an entry that outgrew its first buffer left behind on the way up. */
  private final val SpareBuffers = 8

  /** A ceiling on what is kept, since `parallelism` comes from a build setting and can be anything. */
  private final val MaxRecycled = 64

  /**
   * How much wider than what it would have allocated a recycled buffer may be. Wide enough that the
   * rounding [[rounded]] already does never turns a buffer away, narrow enough that what a buffer
   * measures stays within a factor of what it carries — which is what the in-flight window counts.
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
   * Buffer capacities are rounded so that entries of similar size take each other's buffers. To a
   * power of two while that is cheap, and to whole blocks once it is not: a power of two would leave
   * the buffer an 8 MB entry deflates into with another 8 MB never used.
   */
  private def rounded(atLeast: Int): Int =
    if (atLeast <= MinBufferBytes) MinBufferBytes
    else if (atLeast <= HoldBlockBytes) Integer.highestOneBit(atLeast - 1) << 1
    else if (atLeast > Int.MaxValue - HoldBlockBytes) atLeast
    else (atLeast + HoldBlockBytes - 1) / HoldBlockBytes * HoldBlockBytes

  // ── data classes ─────────────────────────────────────────────────────

  /**
   * One entry's record, as `ZipOutputStream.XEntry` is: what its headers say, and where its local
   * one went. The crc and the two sizes are what the caller declared until deflation replaces them,
   * which happens only for an entry whose flag sends them to a data descriptor.
   */
  private final class XEntry(
      val name: Array[Byte],
      val comment: Array[Byte],
      val reference: ZipReference.Header,
      val method: Int,
      val flag: Int,
      var crc: Long,
      var csize: Long,
      var size: Long
  ) {
    var offset = 0L
  }

  /** An entry being deflated elsewhere, with everything its append owes back to the free lists. */
  private final class Pending(
      val entry: XEntry,
      val future: CompletableFuture[Array[Byte]],
      val input: Array[Array[Byte]],
      val bytes: Long,
      val deflater: Deflater,
      val level: Int
  )

  // ── deflation ────────────────────────────────────────────────────────

  /**
   * Deflates one entry, off the writing thread. Fills in what only deflation knows — the crc and
   * both sizes — and returns the buffer holding the result, `into` unless the bound fell short.
   */
  private def deflateInto(
      x: XEntry,
      deflater: Deflater,
      data: Array[Array[Byte]],
      used: Array[Int],
      len: Int,
      into: Array[Byte],
      window: Int
  ): Array[Byte] = {
    val sum = new CRC32
    var out = into
    var n = 0
    def room(): Unit =
      if (n == out.length) {
        val half = out.length >> 1
        out = Arrays.copyOf(out, out.length + half + MinGrowthBytes)
      }
    // block by block, which the entry is in anyway: this deflater sees the whole entry and finishes
    // it, so only the room its output is given frames what it emits
    var b = 0
    while (b < data.length) {
      sum.update(data(b), 0, used(b))
      deflater.setInput(data(b), 0, used(b))
      while (!deflater.needsInput()) {
        room()
        n += deflater.deflate(out, n, math.min(window, out.length - n))
      }
      b += 1
    }
    deflater.finish()
    while (!deflater.finished()) {
      room()
      n += deflater.deflate(out, n, math.min(window, out.length - n))
    }
    x.crc = sum.getValue
    x.size = len.toLong
    x.csize = n.toLong
    out
  }

  private def await(future: CompletableFuture[Array[Byte]]): Array[Byte] =
    try future.get()
    catch {
      case e: ExecutionException =>
        e.getCause match {
          case null  => throw e
          case cause => throw cause
        }
      // `get` throws a checked exception no `OutputStream` caller can catch, and clears the flag on
      // its way out. Both are put back: sbt cancels a task by interrupting it
      case interrupted: InterruptedException =>
        Thread.currentThread().interrupt()
        val failed = new InterruptedIOException("interrupted while deflating")
        val _ = failed.initCause(interrupted)
        throw failed
    }

  // ── names and comments ───────────────────────────────────────────────

  private def utf8(name: String): Array[Byte] = name.getBytes(StandardCharsets.UTF_8)

  private def commentBytes(e: ZipEntry): Array[Byte] =
    e.getComment match {
      case null    => Array.emptyByteArray
      case comment => utf8(comment)
    }
}
