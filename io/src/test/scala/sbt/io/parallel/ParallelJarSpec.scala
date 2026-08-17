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
import java.util.jar.JarOutputStream
import java.util.zip.{ ZipEntry, ZipException, ZipOutputStream }
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.ZipTestSupport.{ firstExtra, sameBytes }
import ParallelJarOutputStream.JarMagicExtra

/**
 * The jar writer against `JarOutputStream`: the bytes, and the `0xCAFE` extra field it stamps a
 * jar's first entry with — where the caller set an extra field of their own, where the magic is
 * already there, and where that first entry is refused after the magic has been spent on it.
 */
class ParallelJarSpec extends AnyFunSuite with ParallelZipSupport {

  /** The first local header's extra field, after writing one entry that carries `field`. */
  private def extraWritten(field: Array[Byte])(make: ByteArrayOutputStream => ZipSink): Seq[Byte] =
    firstExtra(archive(make) { w =>
      val e = entry("a.txt")
      e.setExtra(field)
      w.putNextEntry(e)
      w.write(oneByte, 0, 1)
      w.closeEntry()
    })

  test("ParallelJarOutputStream writes the same bytes as JarOutputStream") {
    val files = Seq(
      ("META-INF/MANIFEST.MF", "Manifest-Version: 1.0\r\n\r\n".getBytes("UTF-8"), stamp),
      ("a.txt", ("hello " * 100).getBytes("UTF-8"), stamp)
    )
    val a = new ByteArrayOutputStream
    writeThrough(parallelJar(a), Nil, files, stamp)
    val b = new ByteArrayOutputStream
    writeThrough(new JarOutputStream(b), Nil, files, stamp)
    sameBytes("ParallelJarOutputStream", a.toByteArray, b.toByteArray, "JarOutputStream")
  }

  test("ParallelJarOutputStream stamps only the first entry") {
    val files = Seq(
      ("a.txt", "a".getBytes("UTF-8"), stamp),
      ("b.txt", "b".getBytes("UTF-8"), stamp)
    )
    val a = new ByteArrayOutputStream
    writeThrough(parallelJar(a), Nil, files, stamp)
    val bytes = a.toByteArray
    assert(firstExtra(bytes) === JarMagicExtra.toSeq, "first entry lost its magic")
    // exactly one entry carries an extra field, so the archive holds only the four magic bytes
    val magicCount = (0 to bytes.length - JarMagicExtra.length).count { at =>
      JarMagicExtra.indices.forall(i => bytes(at + i) == JarMagicExtra(i))
    }
    assert(magicCount === 2, s"expected the magic in one local header and one central record")
  }

  test("the jar writer prepends its magic to an extra field the caller set") {
    val field = Array(0x11.toByte, 0x22.toByte, 0x00.toByte, 0x00.toByte)
    val expected = extraWritten(field)(new JarOutputStream(_))
    assert(
      extraWritten(field)(parallelJar(_)) === expected,
      "the jar writer must stamp the magic the way JarOutputStream does"
    )
  }

  test("the jar writer does not stamp its magic twice") {
    val expected = extraWritten(JarMagicExtra)(new JarOutputStream(_)) // already stamped
    assert(
      extraWritten(JarMagicExtra)(parallelJar(_)) === expected,
      s"expected $expected, so the magic was stamped onto a field that already had it"
    )
  }

  test("hasMagic agrees with the JDK at every extra field length") {
    // the JDK reads an id before its size and lets a truncated fragment end the walk, so a
    // malformed field whose first two bytes are the magic counts as already stamped
    assert(ParallelJarOutputStream.hasMagic(Array(0xfe.toByte, 0xca.toByte)))
    assert(ParallelJarOutputStream.hasMagic(Array(0xfe.toByte, 0xca.toByte, 0x00.toByte)))
    assert(
      ParallelJarOutputStream.hasMagic(
        JarMagicExtra
      )
    )
    // a different id, correctly sized, followed by the magic
    assert(
      ParallelJarOutputStream.hasMagic(
        Array(0x11.toByte, 0x22.toByte, 0x00.toByte, 0x00.toByte, 0xfe.toByte, 0xca.toByte)
      )
    )
    assert(!ParallelJarOutputStream.hasMagic(Array.emptyByteArray))
    assert(!ParallelJarOutputStream.hasMagic(Array(0x11.toByte)))
    assert(
      !ParallelJarOutputStream.hasMagic(Array(0x11.toByte, 0x22.toByte, 0x00.toByte, 0x00.toByte))
    )
    // a field whose declared size runs past the end must not loop or match
    assert(
      !ParallelJarOutputStream.hasMagic(Array(0x11.toByte, 0x22.toByte, 0xff.toByte, 0xff.toByte))
    )
  }

  test("the jar magic is spent on a refused first entry, as the reference spends it") {
    // `JarOutputStream.putNextEntry` stamps before it defers to `ZipOutputStream`, so an entry
    // refused for any other reason has still used up the archive's one magic field
    // a divergence here means the magic did not outlive the refused entry
    sameAsReference(
      "the jar magic over a refused first entry",
      parallelJar(_),
      new JarOutputStream(_)
    ) { w =>
      val bad = new ZipEntry("bad/")
      bad.setTime(stamp)
      bad.setMethod(ZipEntry.STORED) // no size or crc, so it is refused
      intercept[ZipException](w.putNextEntry(bad))
      writeOne(w, "a.txt")
    }
  }
}
