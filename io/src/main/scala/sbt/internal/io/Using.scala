/* sbt
 * Copyright 2009-2015 Typesafe, Inc, Mark Harrah, and others
 */
package sbt.internal.io

import java.io.{ Closeable, File, FileInputStream, FileOutputStream, InputStream, OutputStream }
import java.io.{ BufferedInputStream, BufferedOutputStream, ByteArrayOutputStream, InputStreamReader, OutputStreamWriter }
import java.io.{ BufferedReader, BufferedWriter, FileReader, FileWriter, Reader, Writer }
import java.util.zip.{ GZIPInputStream, GZIPOutputStream }
import java.net.{ URL, URISyntaxException }
import java.nio.charset.{ Charset, CharsetDecoder, CharsetEncoder }
import java.nio.channels.FileChannel
import java.util.jar.{ Attributes, JarEntry, JarFile, JarInputStream, JarOutputStream, Manifest }
import java.util.zip.{ GZIPOutputStream, ZipEntry, ZipFile, ZipInputStream, ZipOutputStream }

import sbt.io.IO
import ErrorHandling.translate
import Using._

private[sbt] abstract class Using[Source, T] {
  protected def open(src: Source): T
  def apply[R](src: Source)(f: T => R): R =
    {
      val resource = open(src)
      try { f(resource) }
      finally { close(resource) }
    }
  protected def close(out: T): Unit
}
import scala.reflect.{ Manifest => SManifest }
private[sbt] abstract class WrapUsing[Source, T](implicit srcMf: SManifest[Source], targetMf: SManifest[T]) extends Using[Source, T] {
  protected def label[S](m: SManifest[S]) = m.runtimeClass.getSimpleName
  protected def openImpl(source: Source): T
  protected final def open(source: Source): T =
    translate("Error wrapping " + label(srcMf) + " in " + label(targetMf) + ": ") { openImpl(source) }
}
private[sbt] trait OpenFile[T] extends Using[File, T] {
  protected def openImpl(file: File): T
  protected final def open(file: File): T =
    {
      val parent = file.getParentFile
      if (Option(parent).isDefined)
        IO.createDirectory(parent)
      openImpl(file)
    }
}
private[sbt] object Using {
  def wrap[Source, T <: Closeable](openF: Source => T)(implicit srcMf: SManifest[Source], targetMf: SManifest[T]): Using[Source, T] =
    wrap(openF, closeCloseable)
  def wrap[Source, T](openF: Source => T, closeF: T => Unit)(implicit srcMf: SManifest[Source], targetMf: SManifest[T]): Using[Source, T] =
    new WrapUsing[Source, T] {
      def openImpl(source: Source) = openF(source)
      def close(t: T) = closeF(t)
    }

  def resource[Source, T <: Closeable](openF: Source => T): Using[Source, T] =
    resource(openF, closeCloseable)
  def resource[Source, T](openF: Source => T, closeF: T => Unit): Using[Source, T] =
    new Using[Source, T] {
      def open(s: Source) = openF(s)
      def close(s: T) = closeF(s)
    }
  def file[T <: Closeable](openF: File => T): OpenFile[T] = file(openF, closeCloseable)
  def file[T](openF: File => T, closeF: T => Unit): OpenFile[T] =
    new OpenFile[T] {
      def openImpl(file: File) = openF(file)
      def close(t: T) = closeF(t)
    }
  private def closeCloseable[T <: Closeable]: T => Unit = _.close()

  private val gzipBufferSize = 8192
  def bufferedOutputStream: Using[OutputStream, BufferedOutputStream] = wrap((out: OutputStream) => new BufferedOutputStream(out))
  def bufferedInputStream: Using[InputStream, BufferedInputStream] = wrap((in: InputStream) => new BufferedInputStream(in))
  def fileOutputStream(append: Boolean = false): OpenFile[BufferedOutputStream] = file(f => new BufferedOutputStream(new FileOutputStream(f, append)))
  def fileInputStream: OpenFile[BufferedInputStream] = file(f => new BufferedInputStream(new FileInputStream(f)))
  def urlInputStream: Using[URL, BufferedInputStream] = resource((u: URL) => translate("Error opening " + u + ": ")(new BufferedInputStream(u.openStream)))
  def fileOutputChannel: OpenFile[FileChannel] = file(f => new FileOutputStream(f).getChannel)
  def fileInputChannel: OpenFile[FileChannel] = file(f => new FileInputStream(f).getChannel)
  def fileWriter(charset: Charset = IO.utf8, append: Boolean = false): OpenFile[BufferedWriter] =
    file(f => new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f, append), charset)))
  def fileReader(charset: Charset): OpenFile[BufferedReader] = file(f => new BufferedReader(new InputStreamReader(new FileInputStream(f), charset)))
  def urlReader(charset: Charset): Using[URL, BufferedReader] = resource((u: URL) => new BufferedReader(new InputStreamReader(u.openStream, charset)))
  def jarFile(verify: Boolean): OpenFile[JarFile] = file(f => new JarFile(f, verify), (_: JarFile).close())
  def zipFile: OpenFile[ZipFile] = file(f => new ZipFile(f), (_: ZipFile).close())
  def streamReader: Using[(InputStream, Charset), InputStreamReader] =
    wrap { (_: (InputStream, Charset)) match { case (in, charset) => new InputStreamReader(in, charset) } }
  def gzipInputStream: Using[InputStream, GZIPInputStream] =
    wrap((in: InputStream) => new GZIPInputStream(in, gzipBufferSize))
  def zipInputStream: Using[InputStream, ZipInputStream] = wrap((in: InputStream) => new ZipInputStream(in))
  def zipOutputStream: Using[OutputStream, ZipOutputStream] = wrap((out: OutputStream) => new ZipOutputStream(out))
  def gzipOutputStream: Using[OutputStream, GZIPOutputStream] =
    wrap((out: OutputStream) => new GZIPOutputStream(out, gzipBufferSize), (_: GZIPOutputStream).finish())
  def jarOutputStream: Using[OutputStream, JarOutputStream] = wrap((out: OutputStream) => new JarOutputStream(out))
  def jarInputStream: Using[InputStream, JarInputStream] = wrap((in: InputStream) => new JarInputStream(in))
  def zipEntry(zip: ZipFile): Using[ZipEntry, InputStream] = resource((entry: ZipEntry) =>
    translate("Error opening " + entry.getName + " in " + zip + ": ") { zip.getInputStream(entry) })
}
