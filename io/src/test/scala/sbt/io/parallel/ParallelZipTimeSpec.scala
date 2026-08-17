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
import java.nio.file.attribute.FileTime
import java.time.{ LocalDateTime, ZoneId }
import java.util.jar.JarOutputStream
import java.util.concurrent.TimeUnit
import java.util.zip.{ ZipEntry, ZipInputStream, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.ZipTestSupport.firstDifference
import ZipConstants.{ u16, LocBytes, LocExtraLengthOffset, LocNameLengthOffset }

/**
 * Timestamps and the extra fields that carry them: the MS-DOS field and every second it holds, what
 * the reference adds once a time will not fit it, times that arrived through `setExtra` or
 * `setLastModifiedTime`, and an entry carrying no time at all.
 */
class ParallelZipTimeSpec extends AnyFunSuite with ParallelZipSupport {

  /** Mid-year in the local zone, since which side of the DOS ceiling a date falls on depends on it. */
  private def localMillis(year: Int): Long =
    LocalDateTime.of(year, 6, 1, 0, 0).atZone(ZoneId.systemDefault()).toInstant.toEpochMilli

  /** An NTFS timestamp extra field, which `ZipEntry.setExtra` parses and takes the entry's times from. */
  private def ntfs(millis: Long): Array[Byte] = {
    val x = new Array[Byte](36)
    x(0) = 0x0a; x(2) = 32 // 0x000a, 32 bytes
    x(8) = 0x01; x(10) = 24 // tag 1, 24 bytes
    // windows time: 100ns ticks since 1601-01-01, which is this many ticks before the unix epoch
    val windows = millis * 10000L + 116444736000000000L
    (0 until 8).foreach(i => x(12 + i) = ((windows >>> (8 * i)) & 0xff).toByte)
    x
  }

  /** Whether a byte falls in one of the two MS-DOS time fields, the only ones a stamp can move. */
  private def isDosTimeByte(at: Int, archive: Array[Byte]): Boolean = {
    val nameLength = u16(archive, LocNameLengthOffset)
    val extraLength = u16(archive, LocExtraLengthOffset)
    // the local header, one byte of content, then the descriptor
    val centralAt = LocBytes + nameLength + extraLength + 1 + 16
    (at >= 10 && at < 14) || (at >= centralAt + 12 && at < centralAt + 16)
  }

  /**
   * One entry written by each writer, which have to agree byte for byte. Rebuilt per writer, since
   * opening an entry fills fields in on it.
   */
  private def sameOneEntry(what: String)(make: => ZipEntry): Unit =
    sameAsReference(what) { w =>
      w.putNextEntry(make)
      w.write(oneByte, 0, 1)
      w.closeEntry()
    }

  /** The same, at a timestamp, for the sweeps that need the bytes rather than an assertion. */
  private def stampedWith(millis: Long)(make: ByteArrayOutputStream => ZipSink): Array[Byte] =
    archive(make) { w =>
      val e = new ZipEntry("a.txt")
      e.setTime(millis)
      e.setSize(1)
      w.putNextEntry(e)
      w.write(oneByte, 0, 1)
      w.closeEntry()
    }

  test(
    "a timestamp the MS-DOS encoding cannot hold gets the reference's extra field, byte for byte"
  ) {
    // the reference keeps such a time in an extra field rather than lose it: an info-zip one, or an NTFS
    // one past what 32 bit seconds hold. Both edges are here, including its truncating cast below 1901.
    val times = Seq(
      "1970, the unix epoch" -> 0L,
      "half a second before the epoch" -> -500L,
      "a second and a half before" -> -1500L,
      "1979, just inside the DOS floor" -> -31536000000L,
      "1901, where 32 bit seconds run out" -> -2177452800000L,
      "1800, where the reference truncates" -> -5364662400000L,
      // the reference keeps the MS-DOS fields to the end of local 2099, past the bound its own
      // constant names, so an extra field here would be four bytes the reference never wrote
      "2043, the last year the packed field stays positive" -> localMillis(2043),
      "2044, the first year it does not" -> localMillis(2044),
      "2097, the last of those" -> localMillis(2097),
      "2098, past the bound but inside the DOS years" -> localMillis(2098),
      "2099, the last of them" -> localMillis(2099),
      "2100, past the DOS ceiling" -> 4102444800000L,
      "2200, further past it" -> 7258118400000L
    )
    times.foreach { case (label, millis) =>
      sameOneEntry(label) {
        val e = new ZipEntry("a.txt")
        e.setTime(millis)
        e.setSize(1)
        e
      }
    }
  }

  test("every second of every year the MS-DOS fields hold is stamped as the reference stamps it") {
    // from 2044 the seven-bit year reaches the sign bit of the packed field, and JDK 8's `getTime` then
    // returns a millisecond less than it was given. `getTime` is all a subclass sees, so re-encoding it
    // would land a whole two-second unit low — the writer has to recover the field the reference wrote
    val divergent = for {
      year <- 1980 to 2099
      second <- Seq(0, 1, 30, 59) // both parities, since the encoding keeps seconds to two
      millis = LocalDateTime
        .of(year, 6, 1, 12, 34, second)
        .atZone(ZoneId.systemDefault())
        .toInstant
        .toEpochMilli
      ours = stampedWith(millis)(parallelZip(_))
      reference = stampedWith(millis)(new ZipOutputStream(_))
      if !java.util.Arrays.equals(ours, reference)
    } yield s"$year at :$second (${firstDifference(ours, reference)})"
    // the count, not the vector: a regression here diverges for half a century of years at once
    assert(divergent.size === 0, s"first few: ${divergent.take(5).mkString(", ")}")
  }

  test("an extra field the caller set is written as the reference writes it, byte for byte") {
    // the reference drops a zip64 or info-zip header the caller set and writes its own, keeps everything
    // else including an NTFS one, and passes a trailing fragment too short to carry a length straight through
    val fields = Seq[(String, Array[Byte])](
      "one unremarkable field" -> Array[Byte](0x11, 0x22, 0x02, 0x00, 0xaa.toByte, 0xbb.toByte),
      "a zip64 header, which the reference drops" -> Array[Byte](0x01, 0x00, 0x04, 0x00, 1, 2, 3,
        4),
      "an info-zip header, which it drops too" -> Array[Byte](0x55, 0x54, 0x02, 0x00, 7, 8),
      "an NTFS header, which it keeps" -> (Array[Byte](0x0a, 0x00, 0x20, 0x00) ++ new Array[Byte](
        32
      )),
      "all four together" -> (Array[Byte](0x01, 0x00, 0x04, 0x00, 1, 2, 3, 4) ++
        Array[Byte](0x55, 0x54, 0x02, 0x00, 7, 8) ++
        (Array[Byte](0x0a, 0x00, 0x20, 0x00) ++ new Array[Byte](32)) ++
        Array[Byte](0x34, 0x12, 0x01, 0x00, 9)),
      "a trailing fragment with no room for its length" -> Array[Byte](
        0x99.toByte,
        0x99.toByte,
        0x7f,
        0x00,
        1
      )
    )
    fields.foreach { case (what, field) =>
      sameOneEntry(what) {
        val e = entry("a.txt")
        e.setExtra(field.clone())
        e
      }
    }
  }

  test(
    "an entry whose times were set another way is written as the reference writes it, beside a plain one"
  ) {
    // each of these leaves `getTime` exactly where a plain entry's is and still changes the header the
    // reference writes, so an archive holding both is where treating the two alike would show
    val at = FileTime.from(stamp, TimeUnit.MILLISECONDS)
    val perturbations = Seq[(String, ZipEntry => Unit)](
      "setExtra with an NTFS field at the same instant" -> (_.setExtra(ntfs(stamp))),
      "setLastModifiedTime at the same instant" -> { e =>
        val _ = e.setLastModifiedTime(at)
      },
      "setCreationTime" -> { e =>
        val _ = e.setCreationTime(at)
      },
      "setLastAccessTime" -> { e =>
        val _ = e.setLastAccessTime(at)
      }
    )
    perturbations.foreach { case (what, perturb) =>
      sameAsReference(what) { w =>
        // a plain entry, then a perturbed one carrying the same timestamp
        Seq(false, true).foreach { perturbed =>
          val e = new ZipEntry(if (perturbed) "b.txt" else "a.txt")
          e.setTime(stamp)
          if (perturbed) {
            perturb(e)
            assert(
              e.getTime === stamp,
              s"$what moved the time, so this no longer tests two entries reporting the same one"
            )
          }
          w.putNextEntry(e)
          w.write(oneByte, 0, 1)
          w.closeEntry()
        }
      }
    }
  }

  test(
    "an entry whose reported time stopped describing its header is still written as the reference does"
  ) {
    // `setExtra` parses a 0x000a or 0x5455 field and takes the modification time from it while the writer
    // keeps packing the one `setTime` left, so what the entry reports and what its header says come apart.
    // `setLastModifiedTime` does it with no extra field at all. Measured on 8, 21 and 26
    val infoZip = Array[Byte](0x55, 0x54, 0x05, 0x00, 0x01, 0, 0, 0, 0) // mtime 1970
    val perturbed = Seq[(String, ZipEntry => Unit)](
      "setTime then setExtra(NTFS)" -> { e => e.setTime(stamp); e.setExtra(ntfs(0L)) },
      "setExtra(NTFS) then setTime" -> { e => e.setExtra(ntfs(0L)); e.setTime(stamp) },
      "setTime then setExtra(info-zip)" -> { e => e.setTime(stamp); e.setExtra(infoZip) },
      "setLastModifiedTime, no extra field" -> { e =>
        val _ = e.setLastModifiedTime(FileTime.from(stamp, TimeUnit.MILLISECONDS))
      },
      "setLastModifiedTime after setTime" -> { e =>
        e.setTime(0L)
        val _ = e.setLastModifiedTime(FileTime.from(stamp, TimeUnit.MILLISECONDS))
      }
    )
    perturbed.foreach { case (what, configure) =>
      sameOneEntry(what) {
        val e = new ZipEntry("a.txt")
        configure(e)
        e
      }
    }
  }

  test("a lossy timestamp and a caller's extra field are ordered as the reference orders them") {
    val callerExtra = Array[Byte](0x11, 0x22, 0x02, 0x00, 0xaa.toByte, 0xbb.toByte)
    Seq[(String, ByteArrayOutputStream => ZipSink, ByteArrayOutputStream => ZipSink)](
      ("zip", parallelZip(_), new ZipOutputStream(_)),
      ("jar", parallelJar(_), new JarOutputStream(_))
    ).foreach { case (label, ourWriter, referenceWriter) =>
      sameAsReference(label, ourWriter, referenceWriter) { w =>
        val e = new ZipEntry("a.txt")
        e.setTime(0L) // before 1980, so the timestamp field is written too
        e.setSize(1)
        e.setExtra(callerExtra)
        w.putNextEntry(e)
        w.write(oneByte, 0, 1)
        w.closeEntry()
      }
    }
  }

  test("an entry with no timestamp is stamped with the current one, as the reference stamps it") {
    val bytes = archive(parallelZip(_)) { w =>
      val undated = new ZipEntry("a.txt")
      undated.setSize(1)
      assert(undated.getTime === -1L, "this test needs an entry whose time was never set")
      w.putNextEntry(undated)
      w.write(oneByte, 0, 1)
      w.closeEntry()
    }
    assert(namesIn(bytes) === List("a.txt"))
    // and the archive carries a real date rather than the pre-1980 sentinel
    val zis = new ZipInputStream(new ByteArrayInputStream(bytes))
    try assert(zis.getNextEntry.getTime > 1262304000000L, "the entry should carry the current time")
    finally zis.close()
  }

  test("an entry the reference would stamp is stamped, and one it would not is left alone") {
    // the reference tests a field `getTime` does not report: an entry whose modification time is
    // exactly -1 ms reports -1 and yet carries a time, and one given a timestamp only through
    // `setExtra` reports a time and yet carries none. Reading `getTime` gets both backwards.
    val extt = Array(
      0x55.toByte,
      0x54.toByte,
      0x05.toByte,
      0x00.toByte,
      0x01.toByte,
      0x00.toByte,
      0x00.toByte,
      0x00.toByte,
      0x00.toByte
    )
    val cases = Seq[(String, () => ZipEntry)](
      "a modification time of exactly -1 ms" -> { () =>
        val e = new ZipEntry("a.txt")
        e.setLastModifiedTime(FileTime.fromMillis(-1))
        e
      },
      "a time that arrived through setExtra only" -> { () =>
        val e = new ZipEntry("a.txt")
        e.setExtra(extt)
        e
      }
    )
    cases.foreach { case (what, make) =>
      def written(build: ByteArrayOutputStream => ZipSink): Array[Byte] =
        archive(build) { w =>
          w.putNextEntry(make())
          w.write(oneByte, 0, 1)
          w.closeEntry()
        }
      val x = written(parallelZip(_))
      val y = written(new ZipOutputStream(_))
      // where the reference stamps, it stamps the current time on both sides, so the two MS-DOS
      // fields can straddle a tick; everything either side of them has to agree, length included
      assert(x.length === y.length, s"$what: lengths differ, ${x.length} against ${y.length}")
      val differing = x.indices.filter(i => x(i) != y(i))
      assert(
        differing.forall(isDosTimeByte(_, x)),
        s"$what diverged outside the timestamp at ${firstDifference(x, y)}"
      )
    }
  }
}
