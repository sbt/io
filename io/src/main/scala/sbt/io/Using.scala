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

package sbt
package io

import java.io.{
  BufferedInputStream,
  BufferedOutputStream,
  BufferedReader,
  BufferedWriter,
  File,
  FileInputStream,
  FileOutputStream,
  IOException,
  InputStream,
  InputStreamReader,
  OutputStream,
  OutputStreamWriter,
}
import java.net.URL
import java.nio.charset.Charset
import java.util.jar.{ JarFile, JarInputStream, JarOutputStream }
import java.util.zip.{ GZIPInputStream, _ }

import sbt.internal.io.ErrorHandling.translate

abstract class Using[Source, T] {
  protected def open(src: Source): T
  def apply[R](src: Source)(f: T => R): R = {
    val resource = open(src)
    try {
      f(resource)
    } finally {
      close(resource)
    }
  }
  protected def close(out: T): Unit
}

import scala.reflect.{ Manifest => SManifest }
private[sbt] abstract class WrapUsing[Source, T](implicit
    srcMf: SManifest[Source],
    targetMf: SManifest[T]
) extends Using[Source, T] {
  protected def label[S](m: SManifest[S]) = m.runtimeClass.getSimpleName
  protected def openImpl(source: Source): T
  protected final def open(source: Source): T =
    translate("Error wrapping " + label(srcMf) + " in " + label(targetMf) + ": ")(openImpl(source))
}
private[sbt] trait OpenFile[T] extends Using[File, T] {
  protected def openImpl(file: File): T
  protected final def open(file: File): T = {
    val parent = file.getParentFile
    if (parent != null) {
      try IO.createDirectory(parent)
      catch { case _: IOException => }
    }
    openImpl(file)
  }
}

object Using {
  def wrap[Source, T <: AutoCloseable](openF: Source => T)(implicit
      srcMf: SManifest[Source],
      targetMf: SManifest[T]
  ): Using[Source, T] =
    wrap(openF, closeCloseable)

  def wrap[Source, T](openF: Source => T, closeF: T => Unit)(implicit
      srcMf: SManifest[Source],
      targetMf: SManifest[T]
  ): Using[Source, T] =
    new WrapUsing[Source, T] {
      def openImpl(source: Source) = openF(source)
      def close(t: T) = closeF(t)
    }

  def resource[Source, T <: AutoCloseable](openF: Source => T): Using[Source, T] =
    resource(openF, closeCloseable)

  def resource[Source, T](openF: Source => T, closeF: T => Unit): Using[Source, T] =
    new Using[Source, T] {
      def open(s: Source) = openF(s)
      def close(s: T) = closeF(s)
    }

  def file[T <: AutoCloseable](openF: File => T): OpenFile[T] = file(openF, closeCloseable)

  def file[T](openF: File => T, closeF: T => Unit): OpenFile[T] =
    new OpenFile[T] {
      def openImpl(file: File) = openF(file)
      def close(t: T) = closeF(t)
    }

  private def closeCloseable[T <: AutoCloseable]: T => Unit = _.close()

  val bufferedOutputStream = wrap((out: OutputStream) => new BufferedOutputStream(out))
  val bufferedInputStream = wrap((in: InputStream) => new BufferedInputStream(in))

  def fileOutputStream(append: Boolean = false) =
    file(f => new BufferedOutputStream(new FileOutputStream(f, append)))

  val fileInputStream = file(f => new BufferedInputStream(new FileInputStream(f)))

  val urlInputStream = resource((u: URL) =>
    translate("Error opening " + u + ": ")(new BufferedInputStream(u.openStream))
  )

  val fileOutputChannel = file(f => new FileOutputStream(f).getChannel)
  val fileInputChannel = file(f => new FileInputStream(f).getChannel)

  def fileWriter(charset: Charset = IO.utf8, append: Boolean = false) =
    file(f => new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f, append), charset)))

  def fileReader(charset: Charset) =
    file(f => new BufferedReader(new InputStreamReader(new FileInputStream(f), charset)))

  def urlReader(charset: Charset) =
    resource((u: URL) => new BufferedReader(new InputStreamReader(u.openStream, charset)))

  def jarFile(verify: Boolean) = file(f => new JarFile(f, verify), (_: JarFile).close())
  val zipFile = file(f => new ZipFile(f), (_: ZipFile).close())

  val streamReader = wrap {
    (_: (InputStream, Charset)) match { case (in, charset) => new InputStreamReader(in, charset) }
  }

  val gzipInputStream = wrap((in: InputStream) => new GZIPInputStream(in, 8192))
  val zipInputStream = wrap((in: InputStream) => new ZipInputStream(in))
  val zipOutputStream = wrap((out: OutputStream) => new ZipOutputStream(out))

  val gzipOutputStream =
    wrap((out: OutputStream) => new GZIPOutputStream(out, 8192), (_: GZIPOutputStream).finish())

  val jarOutputStream = wrap((out: OutputStream) => new JarOutputStream(out))
  val jarInputStream = wrap((in: InputStream) => new JarInputStream(in))

  def zipEntry(zip: ZipFile) =
    resource((entry: ZipEntry) =>
      translate("Error opening " + entry.getName + " in " + zip + ": ") {
        zip.getInputStream(entry)
      }
    )
}
