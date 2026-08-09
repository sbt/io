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

import java.io.{ BufferedOutputStream, File, FileOutputStream }
import java.nio.file.Files
import java.util.concurrent.{ CountDownLatch, Executors, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger
import java.util.jar.{ Attributes, JarEntry, JarFile, JarOutputStream, Manifest }
import java.util.zip.{ ZipEntry, ZipException, ZipFile, ZipOutputStream }
import scala.collection.immutable.TreeSet
import scala.concurrent.ExecutionContext
import org.scalatest.funsuite.AnyFunSuite
import sbt.io.parallel.ParallelJarOutputStream.JarMagicExtra
import sbt.io.parallel.ParallelZipOutputStream
import sbt.io.parallel.ZipConstants.Zip64EndSig
import sbt.io.syntax._
import ZipTestSupport.{ firstDifference, firstExtra, hasSignature, sameBytes, EmptyCrc }

/**
 * Pins the bytes `IO.zip` and `IO.jar` produce. The references drive `ZipOutputStream` and
 * `JarOutputStream` directly, so what is pinned is the entry walk around them — the directory entries
 * `IO` synthesises, the order it adds them in, and the timestamp it shifts — rather than the writer,
 * and any divergence shows up as unequal bytes rather than as a subtly different archive.
 *
 * Every fixture is then written a third time through `IO.zipParallel` or `IO.jarParallel` and
 * compared with what `IO.zip` and `IO.jar` wrote, so the parallel writers are held to these same
 * archives over all of it. The writers themselves are driven directly by the suites in
 * `sbt.io.parallel`.
 */
class ZipSpec extends AnyFunSuite {

  /**
   * Where the parallel fixtures here deflate. Named `ec` so that the one test naming its own context
   * shadows this rather than competing with it, and taken at each call site rather than inside the
   * helpers, so that a test's context is the one that reaches `IO`.
   */
  private implicit val ec: ExecutionContext = IO.Implicits.deflateContext

  private val fixedTime = Some(1262304000000L) // 2010-01-01T00:00:00Z

  /** The writers take timestamps already shifted the way IO.archive shifts them. */
  private def localOffset: Long = java.util.TimeZone.getDefault.getOffset(1262304000000L).toLong
  private def inDosRange: Long = 1262304000000L - localOffset

  test("zip writes the same bytes as a direct ZipOutputStream, one small entry") {
    checkAgainstReference(fixedTime) { dir =>
      write(dir / "a.txt", "hello " * 100)
      Seq(dir / "a.txt")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, empty entry") {
    checkAgainstReference(fixedTime) { dir =>
      write(dir / "empty.txt", "")
      Seq(dir / "empty.txt")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, incompressible entry") {
    checkAgainstReference(fixedTime) { dir =>
      write(dir / "tiny.txt", "no")
      Seq(dir / "tiny.txt")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, entry spanning many blocks") {
    checkAgainstReference(fixedTime) { dir =>
      write(dir / "big.txt", scala.util.Random.alphanumeric.take(400000).mkString)
      Seq(dir / "big.txt")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, nested directories") {
    checkAgainstReference(fixedTime) { dir =>
      write(dir / "pkg" / "sub" / "a.class", "class A " * 50)
      write(dir / "pkg" / "b.class", "class B " * 50)
      write(dir / "top.txt", "top")
      Seq(dir / "pkg" / "sub" / "a.class", dir / "pkg" / "b.class", dir / "top.txt")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, non-ascii entry name") {
    checkAgainstReference(fixedTime) { dir =>
      // an accented and a wide character, so the name exercises multi-byte utf-8
      write(dir / "\u00e9\u4e2d.txt", "unicode")
      Seq(dir / "\u00e9\u4e2d.txt")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, many entries") {
    checkAgainstReference(fixedTime) { dir =>
      val files = (1 to 200).map { i =>
        val f = dir / "pkg" / s"C$i.class"
        write(f, s"class C$i " * (i % 40 + 1))
        f
      }
      files
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, per-file timestamps") {
    checkAgainstReference(None) { dir =>
      write(dir / "a.txt", "a" * 500)
      write(dir / "b.txt", "b" * 500)
      Seq(dir / "a.txt", dir / "b.txt")
    }
  }

  test("jar writes the same bytes as a direct JarOutputStream") {
    checkJarAgainstReference(fixedTime) { dir =>
      write(dir / "a.txt", "hello " * 100)
      write(dir / "pkg" / "b.class", "class B " * 50)
      Seq(dir / "a.txt", dir / "pkg" / "b.class")
    }
  }

  test("jar writes the same bytes as a direct JarOutputStream, single entry") {
    checkJarAgainstReference(fixedTime) { dir =>
      write(dir / "only.txt", "only")
      Seq(dir / "only.txt")
    }
  }

  test("jar keeps the magic extra field on its first entry") {
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      write(dir / "a.txt", "hello")
      val jar = tmp / "out.jar"
      IO.jar(Seq(dir / "a.txt" -> "a.txt"), jar, new Manifest, fixedTime)

      val bytes = IO.readBytes(jar)
      assert(
        firstExtra(bytes) === JarMagicExtra.toSeq,
        "first jar entry lost its extra field"
      )
    }
  }

  test("jar is readable as a jar") {
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      write(dir / "a.txt", "hello")
      val jar = tmp / "out.jar"
      val mf = new Manifest
      mf.getMainAttributes.put(Attributes.Name.MAIN_CLASS, "Main")
      IO.jar(Seq(dir / "a.txt" -> "a.txt"), jar, mf, fixedTime)

      val jf = new JarFile(jar)
      try {
        assert(jf.getManifest != null, "manifest missing")
        assert(jf.getManifest.getMainAttributes.getValue(Attributes.Name.MAIN_CLASS) === "Main")
        assert(jf.getEntry("a.txt") != null)
      } finally jf.close()
    }
  }

  test("zip rejects two sources mapping to one entry name") {
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      write(dir / "a.txt", "A")
      write(dir / "b.txt", "B")
      intercept[ZipException] {
        IO.zip(Seq(dir / "a.txt" -> "x.txt", dir / "b.txt" -> "x.txt"), tmp / "out.zip", fixedTime)
      }
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, pre-1980 timestamp") {
    checkAgainstReference(Some(0L)) { dir =>
      write(dir / "a.txt", "hello " * 100)
      write(dir / "b.txt", "world " * 100)
      Seq(dir / "a.txt", dir / "b.txt")
    }
  }

  test(
    "zip writes the same bytes as a direct ZipOutputStream, pre-1980 timestamp under directories"
  ) {
    // directory entries are stored rather than deflated, so their timestamp takes a path through the
    // writer that a file's does not
    checkAgainstReference(Some(0L)) { dir =>
      write(dir / "pkg" / "sub" / "a.class", "class A " * 50)
      write(dir / "pkg" / "b.class", "class B " * 50)
      Seq(dir / "pkg" / "sub" / "a.class", dir / "pkg" / "b.class")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, far future timestamp") {
    checkAgainstReference(Some(4102444800000L)) { dir =>
      write(dir / "a.txt", "hello " * 100)
      Seq(dir / "a.txt")
    }
  }

  test("zip writes the same bytes as a direct ZipOutputStream, epoch-zero file times") {
    checkAgainstReference(None) { dir =>
      write(dir / "a.txt", "hello " * 100)
      IO.setModifiedTimeOrFalse(dir / "a.txt", 0L)
      Seq(dir / "a.txt")
    }
  }

  test("zip writes an empty archive") {
    checkAgainstReference(fixedTime)(_ => Nil)
  }

  test("the parallel writer streams an entry too large to hold and still matches the reference") {
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      write(dir / "big.txt", "big " * 40000) // ~160k, streamed against the limit below
      write(dir / "a.txt", "a" * 500)
      write(dir / "pkg" / "b.txt", "b" * 500)
      val sources = Seq(
        dir / "big.txt" -> "big.txt",
        dir / "a.txt" -> "a.txt",
        dir / "pkg" / "b.txt" -> "pkg/b.txt"
      )
      val expected = tmp / "expected.zip"
      referenceZip(sources, expected, fixedTime)

      // drive the writer directly so the streaming threshold can be forced down
      val actual = tmp / "actual.zip"
      val files = sources.map { case (f, n) => (f, n, f.length()) }.sortBy(_._2)
      // a window far larger than the entry limit: the entry must still be streamed, not held
      val fileOut = new BufferedOutputStream(new FileOutputStream(actual), 1 << 16)
      try {
        val w = new ParallelZipOutputStream(fileOut) {
          override protected def windowBytes: Long = 1024L * 1024L
          override protected def maxEntryBytes: Long = 1024L
        }
        val dirEntry = new ZipEntry("pkg/")
        dirEntry.setTime(inDosRange)
        dirEntry.setSize(0)
        dirEntry.setMethod(ZipEntry.STORED)
        dirEntry.setCrc(EmptyCrc)
        w.putNextEntry(dirEntry)
        w.closeEntry()
        files.foreach { case (file, name, size) =>
          val e = new ZipEntry(name)
          e.setTime(inDosRange)
          e.setSize(size)
          w.putNextEntry(e)
          IO.transfer(file, w)
          w.closeEntry()
        }
        w.finish()
        w.close()
      } finally fileOut.close()

      val a = IO.readBytes(actual)
      val b = IO.readBytes(expected)
      assert(
        java.util.Arrays.equals(a, b),
        s"streamed entry diverged: ${a.length} vs ${b.length}, first difference at ${firstDifference(a, b)}"
      )
    }
  }

  test("an archive of more entries than a 16 bit count holds gets zip64, byte for byte") {
    IO.withTemporaryDirectory { tmp =>
      val f = tmp / "one.txt"
      write(f, "x")
      // 65535 is where the reference starts writing a zip64 end record beside the plain one; 65534 does not
      val count = 0xffff
      val sources = (1 to count).map(i => f -> f"e$i%05d.txt")
      val expected = tmp / "expected.zip"
      referenceZip(sources, expected, fixedTime)
      val actual = tmp / "actual.zip"
      IO.zip(sources, actual, fixedTime)

      val a = IO.readBytes(actual)
      sameBytes("IO.zip", a, IO.readBytes(expected), "the reference")
      assert(
        hasSignature(a, Zip64EndSig.toInt),
        "no zip64 end record, where the reference writes one"
      )
      val zf = new ZipFile(actual)
      try assert(zf.size === count, s"wrote ${zf.size} entries of $count")
      finally zf.close()
    }
  }

  test("an entry name past what its length field holds goes the way the reference takes it") {
    // there is no second attempt any more: the writer reproduces what `ZipOutputStream` does with such a
    // name rather than declining it, which up to JDK 21 is to wrap the length and from 22 to refuse.
    IO.withTemporaryDirectory { tmp =>
      val f = tmp / "one.txt"
      write(f, "x")
      val name =
        "\u4e2d" * 22000 // 66000 bytes of utf-8, well under the 65535 characters ZipEntry allows
      val sources = Seq(f -> name)
      def attempt(write: => Unit): Option[String] =
        try { write; None }
        catch { case e: ZipException => Some(e.getMessage) }

      val expected = tmp / "expected.zip"
      val referenceFailure = attempt(referenceZip(sources, expected, fixedTime))
      val actual = tmp / "actual.zip"
      val actualFailure = attempt(IO.zip(sources, actual, fixedTime))
      assert(
        actualFailure === referenceFailure,
        "IO has to fail exactly when and how the reference writer fails"
      )
      if (referenceFailure.isEmpty) {
        val a = IO.readBytes(actual)
        val b = IO.readBytes(expected)
        assert(java.util.Arrays.equals(a, b), s"diverged at ${firstDifference(a, b)}")
      }
    }
  }

  test("zipParallel deflates on a context the caller passed, byte for byte") {
    // the implicit is what a caller has to do to move the deflating off `commonPool`, and every other
    // fixture here imports the one `IO` offers, so this is the one that pins a context reaching `IO` at
    // all. Counted rather than assumed: an archive written on the wrong context has the same bytes, so
    // the comparison alone would pass just as well if nothing the caller said had been read
    val threads = Executors.newFixedThreadPool(3)
    val handovers = new AtomicInteger
    implicit val ec: ExecutionContext = new ExecutionContext {
      def execute(runnable: Runnable): Unit = {
        val _ = handovers.incrementAndGet()
        threads.execute(runnable)
      }
      def reportFailure(cause: Throwable): Unit = throw cause
    }
    try {
      checkAgainstReference(fixedTime) { dir =>
        (1 to 60).map { i =>
          val f = dir / "pkg" / s"C$i.class"
          write(f, s"class C$i " * (i % 40 + 1))
          f
        }
      }
      assert(handovers.get === 60, "the deflating did not go to the context the caller passed")
    } finally { val _ = threads.shutdownNow() }
  }

  test("zipParallel holds no more in flight than the parallelism the caller passed") {
    // the archive is the same at any parallelism, so bytes cannot show whether the parameter arrived.
    // Each handover waits for one more than the limit to arrive: a writer holding to it never trips
    // the latch and peaks at exactly the limit, one ignoring it trips it and peaks above
    val parallelism = 2
    val threads = Executors.newCachedThreadPool()
    val overLimit = new CountDownLatch(parallelism + 1)
    val inFlight = new AtomicInteger
    val peak = new AtomicInteger
    implicit val ec: ExecutionContext = new ExecutionContext {
      def execute(runnable: Runnable): Unit = {
        val held = inFlight.incrementAndGet()
        val _ = peak.getAndUpdate(most => math.max(most, held))
        overLimit.countDown()
        threads.execute { () =>
          val _ = overLimit.await(1, TimeUnit.SECONDS)
          try runnable.run()
          finally { val _ = inFlight.decrementAndGet() }
        }
      }
      def reportFailure(cause: Throwable): Unit = throw cause
    }
    try
      IO.withTemporaryDirectory { dir =>
        val sources = (1 to 6).map { i =>
          val f = dir / s"C$i.class"
          write(f, s"class C$i " * 20)
          f -> s"C$i.class"
        }
        IO.zipParallel(sources, dir / "out.zip", fixedTime, parallelism)
        assert(peak.get === parallelism, s"the writer held ${peak.get} entries in flight at once")
      }
    finally { val _ = threads.shutdownNow() }
  }

  test("zip round-trips through unzip") {
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      write(dir / "pkg" / "a.txt", "contents A")
      write(dir / "b.txt", "")
      val zip = tmp / "out.zip"
      IO.zip(Seq(dir / "pkg" / "a.txt" -> "pkg/a.txt", dir / "b.txt" -> "b.txt"), zip, fixedTime)

      val out = tmp / "extracted"
      IO.unzip(zip, out)
      assert(IO.read(out / "pkg" / "a.txt") === "contents A")
      assert((out / "b.txt").exists)
    }
  }

  test("zip is reproducible for identical input") {
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      write(dir / "a.txt", "hello " * 100)
      val sources = Seq(dir / "a.txt" -> "a.txt")
      val first = tmp / "first.zip"
      val second = tmp / "second.zip"
      IO.zip(sources, first, fixedTime)
      IO.zip(sources, second, fixedTime)
      assert(java.util.Arrays.equals(IO.readBytes(first), IO.readBytes(second)))
    }
  }

  test("zip entry names and contents survive a large tree") {
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      val expected = (1 to 300).map { i =>
        val f = dir / "pkg" / s"C$i.class"
        val body = s"class C$i " * (i % 30 + 1)
        write(f, body)
        (s"pkg/C$i.class", body)
      }
      val zip = tmp / "out.zip"
      IO.zip(
        expected.map { case (name, _) => (dir / "pkg" / name.stripPrefix("pkg/")) -> name },
        zip,
        fixedTime
      )

      val zf = new ZipFile(zip)
      try {
        expected.foreach { case (name, body) =>
          val entry = zf.getEntry(name)
          assert(entry != null, s"$name missing")
          val in = zf.getInputStream(entry)
          try assert(IO.readStream(in) === body)
          finally in.close()
        }
      } finally zf.close()
    }
  }

  // -- helpers ---------------------------------------------------------------

  private def write(file: File, content: String): Unit = IO.write(file, content)

  /**
   * Archives `build`'s files three ways and compares the bytes twice: `IO`'s writer against the
   * reference, which pins the entry walk, and then `IO`'s parallel writer against `IO`'s own, which
   * is the claim that makes the parallel one worth having. Both run over every fixture in this
   * suite rather than over a set of their own, since the two questions are asked of the same
   * archives — the second is only interesting where the first has already passed.
   */
  private def checkArchive(
      what: String,
      archive: (Seq[(File, String)], File) => Unit,
      parallel: (Seq[(File, String)], File) => Unit,
      reference: (Seq[(File, String)], File) => Unit
  )(build: File => Seq[File]): Unit =
    IO.withTemporaryDirectory { tmp =>
      val dir = tmp / "src"
      IO.createDirectory(dir)
      val files = build(dir)
      val sources = files.map(f => f -> IO.relativize(dir, f).get)

      val actual = tmp / s"actual.$what"
      val expected = tmp / s"expected.$what"
      val inParallel = tmp / s"parallel.$what"
      archive(sources, actual)
      reference(sources, expected)
      parallel(sources, inParallel)

      val written = IO.readBytes(actual)
      sameBytes(s"IO.$what", written, IO.readBytes(expected), "the reference")
      sameBytes(s"IO.${what}Parallel", IO.readBytes(inParallel), written, s"IO.$what")
    }

  // the context is taken here rather than read from the suite, so that the one test supplying its own
  // is the one whose context reaches `IO.zipParallel` — resolved where the caller is, not where the
  // call to `IO` happens to be written
  private def checkAgainstReference(
      time: Option[Long]
  )(build: File => Seq[File])(implicit ec: ExecutionContext): Unit =
    checkArchive("zip", IO.zip(_, _, time), IO.zipParallel(_, _, time), referenceZip(_, _, time))(
      build
    )

  private def checkJarAgainstReference(
      time: Option[Long]
  )(build: File => Seq[File])(implicit ec: ExecutionContext): Unit =
    checkArchive(
      "jar",
      IO.jar(_, _, new Manifest, time),
      IO.jarParallel(_, _, new Manifest, time),
      referenceJar(_, _, new Manifest, time)
    )(build)

  /** What IO.jar wrote before deflation was parallelised. */
  private def referenceJar(
      sources: Seq[(File, String)],
      outputJar: File,
      manifest: Manifest,
      time: Option[Long]
  ): Unit = {
    val localTime = time.map(t => t - java.util.TimeZone.getDefault.getOffset(t))
    val main = manifest.getMainAttributes
    if (!main.containsKey(Attributes.Name.MANIFEST_VERSION))
      main.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    val out = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(outputJar)))
    try {
      val m = new ZipEntry(JarFile.MANIFEST_NAME)
      m.setTime(localTime.getOrElse(System.currentTimeMillis))
      out.putNextEntry(m)
      manifest.write(new BufferedOutputStream(out))
      out.closeEntry()
      writeReferenceEntries(sources, out, localTime, new JarEntry(_))
    } finally out.close()
  }

  /** What IO.zip wrote before deflation was parallelised. */
  private def referenceZip(
      sources: Seq[(File, String)],
      outputZip: File,
      time: Option[Long]
  ): Unit = {
    val localTime = time.map(t => t - java.util.TimeZone.getDefault.getOffset(t))
    val out = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(outputZip)))
    try writeReferenceEntries(sources, out, localTime, new ZipEntry(_))
    finally out.close()
  }

  /** Directory entries then file entries, exactly as the sequential writer emits them. */
  private def writeReferenceEntries(
      sources: Seq[(File, String)],
      out: ZipOutputStream,
      localTime: Option[Long],
      createEntry: String => ZipEntry
  ): Unit = {
    val files = sources
      .flatMap { case (file, name) =>
        if (file.isFile) (file, name.replace(File.separatorChar, '/')) :: Nil else Nil
      }
      .sortBy { case (_, name) => name }
    val emptyCRC = EmptyCrc
    val now = System.currentTimeMillis

    def directoryPaths(name: String): List[String] = {
      val components = name.split("/").toList.dropRight(1)
      components.foldLeft(List(""))((acc, c) => (acc.head + c + "/") :: acc).filter(_.length > 1)
    }
    val dirs = TreeSet[String]() ++ files.flatMap { case (_, name) => directoryPaths(name) }

    dirs.foreach { name =>
      val e = createEntry(name)
      e.setTime(localTime.getOrElse(now))
      e.setSize(0)
      e.setMethod(ZipEntry.STORED)
      e.setCrc(emptyCRC)
      out.putNextEntry(e)
      out.closeEntry()
    }
    files.foreach { case (file, name) =>
      val e = createEntry(name)
      e.setTime(localTime.getOrElse(IO.getModifiedTimeOrZero(file)))
      out.putNextEntry(e)
      out.write(Files.readAllBytes(file.toPath))
      out.closeEntry()
    }
  }
}
