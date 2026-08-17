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

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import java.util.zip.{ Deflater, ZipEntry, ZipException, ZipInputStream, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.IO
import sbt.io.ZipTestSupport.{ crcOf, firstDifference }
import ZipConstants.{ u32, CenAttributesOffset, CenMadeByOffset, CenSig }

/**
 * One entry's own record — method, sizes, name and comment — as the reference writes them. Overflow
 * is [[ParallelZipLimitsSpec]]; times and their extra fields are [[ParallelZipTimeSpec]].
 */
class ParallelZipEntrySpec extends AnyFunSuite with ParallelZipSupport {

  test("ParallelZipOutputStream rejects a repeated entry name") {
    val a = new ByteArrayOutputStream
    val w = parallelZip(a)
    intercept[ZipException] {
      writeThrough(
        w,
        Nil,
        Seq(("x.txt", "A".getBytes("UTF-8"), stamp), ("x.txt", "B".getBytes("UTF-8"), stamp)),
        stamp
      )
    }
  }

  test("both writers reject a stored entry with no size or crc") {
    val bad = new ZipEntry("bad/")
    bad.setTime(stamp)
    bad.setMethod(ZipEntry.STORED) // leaves size and crc at -1

    val sequentialMessage =
      intercept[ZipException](sequential(new ByteArrayOutputStream).putNextEntry(bad)).getMessage
    assert(sequentialMessage.startsWith("STORED entry missing size"))

    val parallelMessage =
      intercept[ZipException](parallel(new ByteArrayOutputStream).putNextEntry(bad)).getMessage
    assert(
      parallelMessage.startsWith("STORED entry missing size"),
      s"the parallel writer must reject what the reference rejects: $parallelMessage"
    )
  }

  test("a stored entry carrying content is written as the reference writes it, byte for byte") {
    val body = ("stored, not deflated. " * 3000).getBytes("UTF-8") // past one buffer either way
    val crc = crcOf(body)
    // the reference fills each of the size and the compressed size from the other, so all three ways of
    // declaring the same entry have to produce the same archive
    val declarations = Seq[(String, ZipEntry => Unit)](
      "size, compressed size and crc" -> { e =>
        e.setSize(body.length.toLong); e.setCompressedSize(body.length.toLong); e.setCrc(crc)
      },
      "size and crc only" -> { e => e.setSize(body.length.toLong); e.setCrc(crc) },
      "compressed size and crc only" -> { e =>
        e.setCompressedSize(body.length.toLong); e.setCrc(crc)
      }
    )
    declarations.foreach { case (what, declare) =>
      sameAsReference(what) { w =>
        val e = new ZipEntry("stored.bin")
        e.setTime(stamp)
        e.setMethod(ZipEntry.STORED)
        declare(e)
        w.putNextEntry(e)
        // the reference fills the sizes in as it opens the entry, and a caller can read them back
        assert(e.getSize === body.length.toLong, s"$what: size was not filled in")
        assert(
          e.getCompressedSize === body.length.toLong,
          s"$what: compressed size was not filled in"
        )
        w.write(body, 0, body.length)
        w.closeEntry()
        // its offset depends on the stored entry having written exactly its size
        writeOne(w, "after.txt")
      }
    }
  }

  test("a malformed stored entry is refused in the reference's own words") {
    val body = "content".getBytes("UTF-8")
    val crc = crcOf(body)
    val good = body.length.toLong
    // every rule the reference applies to a stored entry, on both sides of where it applies it
    val cases = Seq[(String, ZipEntry => Unit, Boolean)](
      ("nothing declared", _ => (), true),
      ("no crc", _.setSize(good), true),
      (
        "size and compressed size disagree",
        { e => e.setSize(good); e.setCompressedSize(good + 1) },
        true
      ),
      (
        "a size larger than the content",
        { e => e.setSize(good + 1); e.setCrc(crc) },
        true
      ),
      (
        "a size smaller than the content",
        { e => e.setSize(good - 1); e.setCrc(crc) },
        true
      ),
      (
        "a crc that is not the content's",
        { e => e.setSize(good); e.setCrc(crc ^ 1L) },
        true
      ),
      (
        "nothing written where a size was declared",
        { e => e.setSize(good); e.setCrc(crc) },
        false
      ),
      ("an empty entry declaring a crc", { e => e.setSize(0); e.setCrc(12345L) }, false)
    )
    cases.foreach { case (what, declare, writeBody) =>
      def message(make: ByteArrayOutputStream => ZipSink): String =
        refusalFrom(make) { w =>
          val e = new ZipEntry("s.bin")
          e.setTime(stamp)
          e.setMethod(ZipEntry.STORED)
          declare(e)
          w.putNextEntry(e)
          if (writeBody) w.write(body, 0, body.length)
          w.closeEntry()
          w.finish()
        }
      assert(message(parallelZip(_)) === message(new ZipOutputStream(_)), what)
    }
  }

  test("a deflated entry declaring all its sizes gets them in the header, byte for byte") {
    val body = ("class A { def f = 1 } " * 40).getBytes("UTF-8")
    def drive(w: ZipSink): Unit = {
      w.putNextEntry(declaring("a.class", body))
      w.write(body, 0, body.length)
      w.closeEntry()
    }
    // the reference puts the sizes in the local header and writes no descriptor for such an entry, so
    // a writer that always set bit 3 would differ in the flag and in 16 bytes. The threshold is varied
    // to pin that it makes no difference
    Seq[(String, ByteArrayOutputStream => ZipSink)](
      "the default threshold" -> (parallelZip(_)),
      "a threshold under the entry" -> (out => holdingUpTo(out, 16L))
    ).foreach { case (how, make) => sameAsReference(how, make)(drive) }
  }

  test("the sizes a deflated entry declares are held to, in the reference's order and words") {
    val body = ("class B " * 30).getBytes("UTF-8")
    val (compressed, crc) = deflatedSizes(body)
    val spoiled = Seq[(String, ZipEntry => Unit)](
      "size" -> (_.setSize(body.length + 1L)),
      "compressed size" -> (_.setCompressedSize(compressed + 1)),
      "crc-32" -> (_.setCrc(crc ^ 1L))
    )
    spoiled.foreach { case (what, spoil) =>
      def message(make: ByteArrayOutputStream => ZipSink): String =
        refusalFrom(make) { w =>
          val e = declaring("a.class", body)
          spoil(e)
          w.putNextEntry(e)
          w.write(body, 0, body.length)
          w.closeEntry()
          w.finish()
        }
      // such an entry is deflated by the reference itself, which is what makes these its words rather
      // than a copy of them; the threshold is varied to pin that neither path re-words the refusal
      val expected = message(new ZipOutputStream(_))
      Seq[(String, ByteArrayOutputStream => ZipSink)](
        "the default threshold" -> (parallelZip(_)),
        "a threshold under the entry" -> (out => holdingUpTo(out, 16L))
      ).foreach { case (how, make) =>
        assert(message(make) === expected, s"a wrong $what, $how")
      }
    }
  }

  test("an entry comment reaches the central record, byte for byte") {
    // the reference writes it there, so dropping it is different bytes rather than a refusal. 65488 is
    // where JDK 26 caps `setComment`, so these stay under it as well as under the field's own 65535 bytes
    Seq("", "a note", "unicode: \u4e2d", "c" * 60000, "\u4e2d" * 20000).foreach { comment =>
      sameAsReference(s"a ${comment.length} character comment") { w =>
        val e = entry("a.txt")
        e.setComment(comment)
        w.putNextEntry(e)
        w.write(oneByte, 0, 1)
        w.closeEntry()
        // a directory entry too, whose comment takes the stored path rather than the deflated one
        val d = directoryEntry("d/")
        d.setComment(comment)
        w.putNextEntry(d)
        w.closeEntry()
      }
    }
  }

  test("an empty directory entry is still accepted") {
    val bytes = archive(parallel(_)) { w =>
      w.putNextEntry(directoryEntry("pkg/"))
      w.closeEntry()
    }
    assert(namesIn(bytes) === List("pkg/"))
  }

  test("a rejected entry leaves nothing behind and its name stays claimable") {
    // a refusal from before the name is claimed, which is where they all are now
    val rejections = Seq[(String, ZipEntry => Unit)](
      "stored with no crc" -> { e => e.setMethod(ZipEntry.STORED); e.setSize(5) },
      "stored where the two sizes disagree" -> { e =>
        e.setMethod(ZipEntry.STORED); e.setSize(5); e.setCompressedSize(6); e.setCrc(0)
      }
    )
    rejections.foreach { case (what, spoil) =>
      val out = new ByteArrayOutputStream
      val w = parallel(out)
      val bad = new ZipEntry("x.txt")
      bad.setTime(stamp)
      spoil(bad)
      intercept[ZipException](w.putNextEntry(bad))

      // the rejection must not have consumed the name
      writeOne(w, "x.txt")
      w.finish()
      w.close()
      assert(
        namesIn(out.toByteArray) === List("x.txt"),
        s"$what: the rejected entry was written anyway, or its name was consumed"
      )
    }
  }

  test("an entry read back from an archive is written the way the reference writes it") {
    // `ZipFile` and `ZipInputStream` fill in the sizes and crc unasked; measured, 21 and later then write
    // them in a descriptor where 8 and 11 write them in the header. Nothing outside `java.util.zip` can
    // read that difference off the entry, which is why the reference is asked
    val body = ("object B { val x = 2 } " * 200).getBytes("UTF-8")
    val source = new ByteArrayOutputStream
    val z = new ZipOutputStream(source)
    val plain = new ZipEntry("b.class")
    plain.setTime(stamp)
    z.putNextEntry(plain)
    z.write(body, 0, body.length)
    z.closeEntry()
    z.close()

    /** The entry as a reader hands it back, which is only complete once its content has been read. */
    def readBack(): ZipEntry = {
      val in = new ZipInputStream(new ByteArrayInputStream(source.toByteArray))
      try {
        val e = in.getNextEntry
        val buf = new Array[Byte](8192)
        Iterator.continually(in.read(buf)).takeWhile(_ > 0).foreach(_ => ())
        in.closeEntry()
        new ZipEntry(e)
      } finally in.close()
    }
    val read = readBack()
    assert(
      read.getSize > 0 && read.getCompressedSize > 0 && read.getCrc > 0,
      "the reader no longer fills in all three, so this tests nothing"
    )

    def repack(
        make: ByteArrayOutputStream => ZipSink,
        level: Int
    ): (Array[Byte], String) = {
      val out = new ByteArrayOutputStream
      val w = make(out)
      w.setLevel(level)
      val refusal =
        try {
          w.putNextEntry(readBack())
          w.write(body, 0, body.length)
          w.closeEntry()
          w.finish()
          ""
        } catch { case e: ZipException => e.getMessage }
      try w.close()
      catch { case _: ZipException => () }
      (out.toByteArray, refusal)
    }

    Seq(Deflater.DEFAULT_COMPRESSION, 1).foreach { level =>
      val (ours, ourRefusal) = repack(parallelZip(_), level)
      val (reference, itsRefusal) = repack(new ZipOutputStream(_), level)
      assert(ourRefusal === itsRefusal, s"at level $level")
      assert(
        java.util.Arrays.equals(ours, reference),
        s"at level $level, diverged at ${firstDifference(ours, reference)}"
      )
    }
  }

  test("an entry carrying a method neither writer supports is refused in the reference's words") {
    // `setMethod` refuses anything but the two, but `ZipInputStream` hands back whatever method the header it
    // read held, and the copy constructor carries it on
    val source = new ByteArrayOutputStream
    val z = new ZipOutputStream(source)
    z.putNextEntry(
      directoryEntry("c.class")
    ) // stored and empty, so its header carries no content to decode
    z.closeEntry()
    z.close()
    val patched = source.toByteArray
    patched(8) = 12 // the local header's compression method, which nothing public sets
    patched(9) = 0
    val read = {
      val in = new ZipInputStream(new ByteArrayInputStream(patched))
      try in.getNextEntry
      finally in.close()
    }
    assert(read.getMethod === 12, "the reader no longer hands back the method its header held")

    def refusal(make: ByteArrayOutputStream => ZipSink): String =
      refusalFrom(make)(_.putNextEntry(new ZipEntry(read)))
    assert(refusal(parallelZip(_)) === refusal(new ZipOutputStream(_)))

    // and the refusal comes where the reference's does, ahead of the name being claimed. Otherwise the
    // header probe reaches the same refusal a moment later, having already left the name behind — which only
    // happens at all for an entry declaring all three sizes, so it is no substitute for refusing here.
    val out = new ByteArrayOutputStream
    val w = parallelZip(out)
    intercept[ZipException](w.putNextEntry(new ZipEntry(read)))
    val corrected = new ZipEntry(read.getName)
    corrected.setTime(stamp)
    w.putNextEntry(corrected)
    w.write("x".getBytes("UTF-8"), 0, 1)
    w.closeEntry()
    w.close()
    assert(
      namesIn(out.toByteArray) === List(read.getName),
      "the refused entry left its name claimed against a corrected second attempt"
    )
  }

  test("an entry that set no method is left carrying the one the writer resolved") {
    def resolved(make: ByteArrayOutputStream => ZipSink): Int = {
      val w = make(new ByteArrayOutputStream)
      val e = new ZipEntry("a.txt")
      e.setTime(stamp)
      try {
        w.putNextEntry(e)
        e.getMethod
      } finally w.close()
    }
    assert(resolved(parallelZip(_)) === resolved(new ZipOutputStream(_)))
  }

  test("an entry from a ZipFile keeps the platform and permissions the reference keeps") {
    // `externalFileAttributes` reaches an entry only through `ZipFile`, and only from a unix central
    // header — which is why the repack test above cannot see this. Both fields live only in the
    // central header, so dropping them is a quietly different archive rather than a refusal
    val body = "hello".getBytes("UTF-8")
    val sum = crcOf(body)
    val plain = new ByteArrayOutputStream
    val seed = new ZipOutputStream(plain)
    val e = new ZipEntry("a.txt")
    e.setTime(stamp)
    e.setMethod(ZipEntry.STORED)
    e.setSize(body.length.toLong)
    e.setCrc(sum)
    seed.putNextEntry(e)
    seed.write(body, 0, body.length)
    seed.closeEntry()
    seed.finish()
    seed.close()

    // the seed archive says MS-DOS and carries no permissions, so both are patched into its central
    // header: platform 3 (unix) in the high byte of "version made by", then rwxr-xr-x
    val patched = plain.toByteArray
    val cen = (0 to patched.length - 4).indexWhere(at => u32(patched, at) == CenSig)
    assert(cen >= 0, "no central header in the seed archive")
    patched(cen + CenMadeByOffset + 1) = 3.toByte
    Seq(0x00, 0x00, 0xed, 0x81).zipWithIndex.foreach { case (b, i) =>
      patched(cen + CenAttributesOffset + i) = b.toByte
    }

    IO.withTemporaryDirectory { dir =>
      val file = new java.io.File(dir, "seed.zip")
      IO.write(file, patched)
      val zf = new java.util.zip.ZipFile(file)
      try {
        val read = zf.entries.nextElement
        // a divergence here means the central header's platform or external attributes were dropped
        sameAsReference("an entry from a ZipFile") { w =>
          val copy = new ZipEntry(read) // the copy constructor is what carries the attributes over
          copy.setMethod(ZipEntry.STORED)
          copy.setSize(body.length.toLong)
          copy.setCompressedSize(body.length.toLong)
          copy.setCrc(sum)
          w.putNextEntry(copy)
          w.write(body, 0, body.length)
          w.closeEntry()
        }
      } finally zf.close()
    }
  }

  test("a stored entry the caller keeps is left as the reference leaves it") {
    // `ZipFile` sets a compressed size without recording that a caller set it, and 21 and later read
    // that record to place a later deflated write's sizes. Compared on what each writer left the
    // entry, refusal and all, since before 21 there is no record to leave
    val body = new Array[Byte](4096)
    new java.util.Random(2).nextBytes(body)
    val sum = crcOf(body)
    IO.withTemporaryDirectory { dir =>
      val seed = new ByteArrayOutputStream
      val z = new ZipOutputStream(seed)
      val first = new ZipEntry("a.bin")
      first.setTime(stamp)
      first.setMethod(ZipEntry.STORED)
      first.setSize(body.length.toLong)
      first.setCrc(sum)
      z.putNextEntry(first)
      z.write(body, 0, body.length)
      z.closeEntry()
      z.finish()
      z.close()
      val file = new java.io.File(dir, "seed.zip")
      IO.write(file, seed.toByteArray)

      /** The same entry written stored by `make`, then handed to the reference again as deflated. */
      def reused(make: ByteArrayOutputStream => ZipSink): Either[String, Array[Byte]] = {
        val zf = new java.util.zip.ZipFile(file)
        val e =
          try new ZipEntry(zf.entries.nextElement)
          finally zf.close()
        e.setMethod(ZipEntry.STORED)
        val w = make(new ByteArrayOutputStream)
        w.putNextEntry(e)
        w.write(body, 0, body.length)
        w.closeEntry()
        w.finish()
        w.close()
        e.setMethod(ZipEntry.DEFLATED)
        val out = new ByteArrayOutputStream
        val again = new ZipOutputStream(out)
        try {
          again.putNextEntry(e)
          again.write(body, 0, body.length)
          again.closeEntry()
          again.finish()
          again.close()
          Right(out.toByteArray)
        } catch { case refused: ZipException => Left(refused.getMessage) }
      }
      (reused(parallelZip(_)), reused(new ZipOutputStream(_))) match {
        case (Right(ours), Right(reference)) =>
          assert(
            java.util.Arrays.equals(ours, reference),
            s"diverged at ${firstDifference(ours, reference)} — the stored write left something behind"
          )
        case (Left(ours), Left(reference)) =>
          assert(ours === reference, "the stored write left a different entry behind")
        case (ours, reference) =>
          fail(
            s"the second write went one way after ours and another after the reference's," +
              s" $ours against $reference"
          )
      }
    }
  }
}
