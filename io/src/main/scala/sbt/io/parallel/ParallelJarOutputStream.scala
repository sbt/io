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

import java.io.OutputStream
import java.util.zip.ZipEntry
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

import ZipConstants.u16

/**
 * The jar flavour, mirroring `JarOutputStream extends ZipOutputStream`: stamps the first entry with the
 * `0xCAFE` magic extra field as `JarOutputStream.putNextEntry` does, and is otherwise identical.
 */
private[sbt] final class ParallelJarOutputStream(
    out: OutputStream,
    parallelism: Int
)(implicit ec: ExecutionContext)
    extends ParallelZipOutputStream(out, parallelism)(ec) {

  private var stamped = false

  override protected def stamp(e: ZipEntry): Unit =
    if (!stamped) {
      stamped = true
      val magic = ParallelJarOutputStream.JarMagicExtra
      // onto the entry, as `JarOutputStream` stamps it: visible to the caller, validated by `setExtra`
      Option(e.getExtra) match {
        case None                                                    => e.setExtra(magic)
        case Some(extra) if !ParallelJarOutputStream.hasMagic(extra) => e.setExtra(magic ++ extra)
        case Some(_)                                                 => ()
      }
    }
}

private[sbt] object ParallelJarOutputStream {

  /** Whether an extra field already carries the jar magic id, walked as `JarOutputStream.hasMagic` walks. */
  private[io] def hasMagic(extra: Array[Byte]): Boolean = {
    // id before size, as the JDK does: a truncated trailing fragment of just the id counts as stamped
    @tailrec def inFieldAt(at: Int): Boolean =
      if (at + IdBytes > extra.length) false
      else if (u16(extra, at) == JarMagicId) true
      else if (at + HeaderBytes > extra.length) false
      else inFieldAt(at + u16(extra, at + IdBytes) + HeaderBytes)
    inFieldAt(0)
  }

  /** 0xCAFE, the id `JarOutputStream` stamps a jar's first entry with. */
  private final val JarMagicId = 0xcafe

  /** The whole field it stamps: the magic id and a zero length, nothing after. */
  private[io] final val JarMagicExtra: Array[Byte] =
    Array(0xfe.toByte, 0xca.toByte, 0x00.toByte, 0x00.toByte)

  /** An extra field's header: a two byte id, then a two byte length of what follows it. */
  private final val IdBytes = 2
  private final val HeaderBytes = 4
}
