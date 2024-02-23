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

package sbt.internal.io

import java.io.File
import sbt.io.JavaMilli

private[sbt] abstract class Milli {
  def getModifiedTime(filePath: String): Long
  def setModifiedTime(filePath: String, mtime: Long): Unit
  def copyModifiedTime(fromFilePath: String, toFilePath: String): Unit
}

// No native time information? Copy just the milliseconds
private[sbt] abstract class MilliMilliseconds extends Milli {
  def copyModifiedTime(fromFilePath: String, toFilePath: String): Unit =
    setModifiedTime(toFilePath, getModifiedTime(fromFilePath))
}

object Milli {
  val milli: Milli = JavaMilli
  def getModifiedTime(file: File): Long =
    milli.getModifiedTime(file.getPath)
  def setModifiedTime(file: File, mtime: Long): Unit =
    milli.setModifiedTime(file.getPath, mtime)
  def copyModifiedTime(fromFile: File, toFile: File): Unit =
    milli.copyModifiedTime(fromFile.getPath, toFile.getPath)
  def getMilliSupportDiagnostic(projectDir: File): Option[String] = None
}
