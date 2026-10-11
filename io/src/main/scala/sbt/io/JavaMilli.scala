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

import java.io.FileNotFoundException
import java.nio.file.attribute.FileTime
import java.nio.file.{ Files, NoSuchFileException, Paths => JPaths }

import sbt.internal.io.MilliMilliseconds

object JavaMilli extends MilliMilliseconds {
  def getModifiedTime(filePath: String): Long =
    // Avoids the by-name thunk of mapNoSuchFileException: this is an allocation hotspot.
    try Files.getLastModifiedTime(JPaths.get(filePath)).toMillis
    catch { case e: NoSuchFileException => throw noSuchFile(e) }

  def setModifiedTime(filePath: String, mtime: Long): Unit =
    mapNoSuchFileException {
      Files.setLastModifiedTime(JPaths.get(filePath), FileTime.fromMillis(mtime))
      ()
    }

  private def mapNoSuchFileException[A](f: => A): A =
    try {
      f
    } catch {
      case e: NoSuchFileException => throw noSuchFile(e)
    }

  private def noSuchFile(e: NoSuchFileException): Throwable =
    new FileNotFoundException(e.getFile).initCause(e)
}
