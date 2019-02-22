/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.io

import java.io.FileNotFoundException
import java.nio.file.{ Files, NoSuchFileException }
import java.nio.file.{ Paths => JPaths }
import java.nio.file.attribute.FileTime

import sbt.internal.io.MilliMilliseconds

object JavaMilli extends MilliMilliseconds {
  def getModifiedTime(filePath: String): Long =
    mapNoSuchFileException(Files.getLastModifiedTime(JPaths.get(filePath)).toMillis)

  def setModifiedTime(filePath: String, mtime: Long): Unit =
    mapNoSuchFileException {
      Files.setLastModifiedTime(JPaths.get(filePath), FileTime.fromMillis(mtime))
      ()
    }

  private def mapNoSuchFileException[A](f: => A): A =
    try {
      f
    } catch {
      case e: NoSuchFileException => throw new FileNotFoundException(e.getFile).initCause(e)
    }
}
