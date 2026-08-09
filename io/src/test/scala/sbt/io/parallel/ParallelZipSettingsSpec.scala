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
import java.util.zip.{ Deflater, ZipEntry }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.ZipTestSupport.halfCompressible

/**
 * The three settings `ZipOutputStream` carries — `setLevel`, `setMethod` and `setComment` — including
 * a level changed part way through an archive or part way through an entry, which the reference lets
 * reach the deflater it is already using.
 */
class ParallelZipSettingsSpec extends AnyFunSuite with ParallelZipSupport {

  /** Writes `body` to the open entry in `chunk`-sized pieces, since where a write ends reaches the bytes. */
  private def writeInChunks(w: ZipSink, body: Array[Byte], chunk: Int): Unit = {
    var off = 0
    while (off < body.length) {
      val n = math.min(chunk, body.length - off)
      w.write(body, off, n)
      off += n
    }
  }

  test("setLevel reaches every deflater, byte for byte") {
    val body = ("class A { def f = 1 } " * 400).getBytes("UTF-8")
    Seq(Deflater.DEFAULT_COMPRESSION, 0, 1, 6, 9).foreach { level =>
      sameAsReference(s"level $level") { w =>
        w.setLevel(level)
        w.putNextEntry(entry("a.txt", body.length))
        w.write(body, 0, body.length)
        w.closeEntry()
      }
    }
    // and an entry already queued keeps the level it was submitted with
    val bytes = archive(new ParallelZipOutputStream(_)) { w =>
      w.setLevel(9)
      writeOne(w, "a.txt")
      w.setLevel(0)
    }
    assert(namesIn(bytes) === List("a.txt"))
  }

  test("an entry past one deflate block is byte for byte at every level, held or streamed") {
    // zlib sizes a stored block by the output room it is given, so at level 0 the buffer the deflater
    // deflates into decides the bytes — the reference's is `DeflaterOutputStream`'s 512. Under about 32 KB
    // it is one block either way, which is why a small body never shows this; 200 KB does.
    val body = halfCompressible(200000, seed = 11)
    Seq(Deflater.NO_COMPRESSION, 1, 3, 6, 9, Deflater.DEFAULT_COMPRESSION).foreach { level =>
      def drive(w: ZipSink): Unit = {
        w.setLevel(level)
        val e = new ZipEntry("a.bin")
        e.setTime(stamp)
        w.putNextEntry(e) // no declared size, so the streamed writer below has to grow into it
        w.write(body, 0, body.length)
        w.closeEntry()
      }
      Seq[(String, ByteArrayOutputStream => ZipSink)](
        "held" -> (new ParallelZipOutputStream(_)),
        "streamed" -> (out => holdingUpTo(out, 4096L))
      ).foreach { case (how, make) => sameAsReference(s"level $level $how", make)(drive) }
    }
  }

  test("a level changed part way through an archive reaches the entries after it, byte for byte") {
    // the writer recycles its deflaters, and one built at another level cannot just be reset into this
    // one, so a change mid-archive is where that would show. The reference reuses a single deflater and
    // changes its level, which is the behaviour these bytes have to match.
    val body = ("class A { def f = 1 } " * 200).getBytes("UTF-8")
    sameAsReference("a level changed mid-archive") { w =>
      Seq(9, 9, 1, 1, 0, 6, 9).zipWithIndex.foreach { case (level, i) =>
        w.setLevel(level)
        w.putNextEntry(entry(s"e$i.txt", body.length))
        w.write(body, 0, body.length)
        w.closeEntry()
      }
    }
  }

  test("setMethod applies to an entry that set none, byte for byte") {
    Seq(ZipEntry.STORED, ZipEntry.DEFLATED).foreach { method =>
      sameAsReference(s"method $method") { w =>
        w.setMethod(method)
        val e = new ZipEntry("d/") // no method of its own, so the writer's default applies
        e.setTime(stamp)
        e.setSize(0)
        e.setCrc(0)
        w.putNextEntry(e)
        w.closeEntry()
      }
    }
  }

  test("setComment reaches the end record, byte for byte") {
    Seq("", "hello", "a comment with unicode: 中", "x" * 0xffff).foreach { comment =>
      sameAsReference(s"a ${comment.length} character comment") { w =>
        w.setComment(comment)
        writeOne(w, "a.txt")
      }
    }
  }

  test("the settings refuse what the reference refuses, in its words") {
    // against the reference's own message rather than a literal: the wording moves between JDKs — 8 and
    // 11 end the comment one with a full stop, 21 and later do not — and matching it is the point
    val cases = Seq[(String, ZipSink => Unit)](
      "a level above the range" -> (_.setLevel(10)),
      "a level below it" -> (_.setLevel(-2)),
      "a method that is neither" -> (_.setMethod(1)),
      "a comment past 64 KB" -> (_.setComment("c" * 0x10000))
    )
    cases.foreach { case (what, misuse) =>
      def refusal(make: OutputStream => ZipSink): String =
        intercept[IllegalArgumentException](misuse(make(new ByteArrayOutputStream))).getMessage
      val reference = refusal(sequential)
      assert(reference != null && reference.nonEmpty, s"$what: the reference said nothing")
      assert(refusal(parallel) === reference, s"$what: unexpected wording")
    }
    // null leaves the comment alone rather than failing, as it does there
    parallel(new ByteArrayOutputStream).setComment(null)
  }

  test("setComment(null) does to a comment already set what the reference does, byte for byte") {
    // clears it on 21 and later, and leaves the last one standing on 8 and 11, which assign only a comment
    // that is not null — so this asserts agreement rather than either behaviour
    sameAsReference("setComment(null)") { w =>
      w.setComment("this should be cleared")
      w.setComment(null)
      writeOne(w, "a.txt")
    }
  }

  test("an uncompressed entry is framed as the reference frames it, however it was written") {
    // at level 0 zlib frames a stored block from how much input has arrived as well as from the
    // room it is handed, so an entry written in pieces is framed differently from the same bytes
    // written at once. Holding it would reproduce the second whatever the caller did.
    val body = ("frame me " * 25000).getBytes("UTF-8") // 225000 bytes, well past one block
    Seq(body.length, 8192, 777).foreach { chunk =>
      sameAsReference(s"level 0 in $chunk-byte writes") { w =>
        w.setLevel(Deflater.NO_COMPRESSION)
        w.putNextEntry(entry("a.bin", body.length))
        writeInChunks(w, body, chunk)
        w.closeEntry()
      }
    }
  }

  test("a level set part way through an entry reaches that entry, byte for byte") {
    // the reference keeps one deflater and `setLevel` reaches it, so the rest of the open entry is
    // written at the new level. Every pair matters: zlib settles the change by flushing a block,
    // and how much room it is given to do that reaches the output at every level, not only at 0.
    //
    // Both bodies matter, and only the second one can fail. What the change flushes is whatever zlib
    // still had pending, and how much that is depends on the room the writes before it were given —
    // so a body has to leave output pending across a write to tell one writer's buffer from another's.
    // A repetitive one deflates away to almost nothing and an incompressible one is stored block for
    // block whatever the room; neither leaves much. Half compressible does, but only if the two are
    // interleaved irregularly: at a fixed stride the blocks land in the same place either way. This
    // one is what a fuzzer over random sizes and strides settled on, kept at the size it found
    val repetitive =
      ("switch me " * 12000).getBytes("UTF-8") // 120000 bytes, past one deflate block
    val patchy = new Array[Byte](120000)
    val rnd = new java.util.Random(3)
    rnd.nextBytes(patchy)
    var z = 0
    while (z < patchy.length) {
      patchy(z) = 0; z += 1 + rnd.nextInt(3)
    }
    val levels = Seq(Deflater.NO_COMPRESSION, 1, 6, 9, Deflater.DEFAULT_COMPRESSION)
    Seq("repetitive" -> repetitive, "patchy" -> patchy).foreach { case (kind, body) =>
      levels.foreach { from =>
        levels.foreach { to =>
          Seq(8192, 999).foreach { chunk =>
            sameAsReference(s"$kind $from to $to in $chunk-byte writes") { w =>
              w.setLevel(from)
              w.putNextEntry(entry("a.bin", body.length))
              var off = 0
              var switched = false
              while (off < body.length) {
                if (!switched && off >= body.length / 2) {
                  w.setLevel(to)
                  switched = true
                }
                val n = math.min(chunk, body.length - off)
                w.write(body, off, n)
                off += n
              }
              w.closeEntry()
            }
          }
        }
      }
    }
  }
}
