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
import java.util.zip.{ ZipEntry, ZipOutputStream }

/**
 * A zip writer to add entries to: what `ZipOutputStream` and [[ParallelZipOutputStream]] both offer,
 * behind one name. The JDK's writer carries a `Deflater` and counters the parallel one never uses,
 * so the parallel one extends this rather than that, and only the JDK's is adapted. `IO`'s entry
 * walk drives the two through it, as do the suites and the benchmark comparing them.
 *
 * An adapter rather than a structural type: Scala 3 dispatches a structural call through
 * `Method.invoke`, which wraps whatever the writer threw in an `InvocationTargetException`, and what
 * a writer throws is most of what those suites are comparing.
 */
private[sbt] abstract class ZipSink extends OutputStream {
  def putNextEntry(e: ZipEntry): Unit
  def closeEntry(): Unit
  def finish(): Unit
  def setLevel(level: Int): Unit
  def setMethod(method: Int): Unit
  def setComment(comment: String): Unit
}

private[sbt] object ZipSink {

  def apply(z: ZipOutputStream): ZipSink = new ZipSink {
    def putNextEntry(e: ZipEntry): Unit = z.putNextEntry(e)
    def closeEntry(): Unit = z.closeEntry()
    def finish(): Unit = z.finish()
    def setLevel(level: Int): Unit = z.setLevel(level)
    def setMethod(method: Int): Unit = z.setMethod(method)
    def setComment(comment: String): Unit = z.setComment(comment)
    def write(b: Int): Unit = z.write(b)
    override def write(b: Array[Byte], off: Int, len: Int): Unit = z.write(b, off, len)
    override def flush(): Unit = z.flush()
    override def close(): Unit = z.close()
  }
}
