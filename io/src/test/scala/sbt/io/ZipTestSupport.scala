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

import java.util.zip.CRC32
import org.scalatest.Assertions.fail
import sbt.io.parallel.ZipConstants.u16

/** Byte-level helpers the archive-writer specs share. */
private[io] object ZipTestSupport {

  /** Two archives that have to be the same, with where they part company when they are not. */
  def sameBytes(what: String, actual: Array[Byte], expected: Array[Byte], from: String): Unit =
    if (!java.util.Arrays.equals(actual, expected))
      fail(
        s"$what diverged from $from: ${actual.length} bytes vs ${expected.length};" +
          s" first difference at ${firstDifference(actual, expected)}"
      )

  def crcOf(b: Array[Byte]): Long = {
    val sum = new CRC32
    sum.update(b)
    sum.getValue
  }

  /** The crc of no bytes at all, which every empty directory entry carries. */
  val EmptyCrc: Long = new CRC32().getValue

  /**
   * Bytes that deflate to about half their size, which is what makes a deflater do real work: an
   * incompressible body is stored block for block and a repetitive one vanishes, and neither tells
   * one writer's framing from another's.
   */
  def halfCompressible(bytes: Int, seed: Long, stride: Int = 2): Array[Byte] = {
    val body = new Array[Byte](bytes)
    new java.util.Random(seed).nextBytes(body)
    body.indices.by(stride).foreach(body(_) = 0)
    body
  }

  /** Whether a little-endian record signature or extra field id appears anywhere in the archive. */
  def hasSignature(archive: Array[Byte], signature: Int, width: Int = 4): Boolean = {
    val want = (0 until width).map(i => ((signature >>> (8 * i)) & 0xff).toByte)
    (0 to archive.length - width).exists(at =>
      (0 until width).forall(i => archive(at + i) == want(i))
    )
  }

  /** Where two archives diverge, for a failure message. */
  def firstDifference(a: Array[Byte], b: Array[Byte]): String = {
    val n = math.min(a.length, b.length)
    (0 until n).find(i => a(i) != b(i)) match {
      case None    => s"byte $n (one is a prefix of the other)"
      case Some(i) => f"byte $i (0x${a(i) & 0xff}%02x vs 0x${b(i) & 0xff}%02x)"
    }
  }

  /** The extra field written into the first local header. */
  def firstExtra(bytes: Array[Byte]): Seq[Byte] = {
    val nameLen = u16(bytes, 26)
    val extraLen = u16(bytes, 28)
    bytes.slice(30 + nameLen, 30 + nameLen + extraLen).toSeq
  }
}
