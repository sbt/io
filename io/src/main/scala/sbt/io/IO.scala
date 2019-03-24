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

import java.io._
import java.net.{ URI, URISyntaxException, URL }
import java.nio.charset.Charset
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{ Path => NioPath, _ }
import java.util.Properties
import java.util.jar.{ Attributes, JarEntry, JarOutputStream, Manifest }
import java.util.zip.{ CRC32, ZipEntry, ZipInputStream, ZipOutputStream }

import sbt.internal.io.ErrorHandling.translate
import sbt.internal.io.{ Milli, Retry }
import sbt.io.Using._
import sbt.nio.{ AllPass, FileTreeView, Glob }

import scala.Function.tupled
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeSet
import scala.collection.mutable.{ HashMap, HashSet }
import scala.reflect.{ Manifest => SManifest }
import scala.util.control.Exception._
import scala.util.control.NonFatal

/** A collection of File, URL, and I/O utility methods.*/
object IO {

  /** The maximum number of times a unique temporary filename is attempted to be created.*/
  private val MaximumTries = 10

  /** The producer of randomness for unique name generation.*/
  private lazy val random = new java.util.Random
  val temporaryDirectory = new File(System.getProperty("java.io.tmpdir"))

  /** The size of the byte or char buffer used in various methods.*/
  private val BufferSize = 8192

  /** File scheme name */
  private[sbt] val FileScheme = "file"

  /** The newline string for this system, as obtained by the line.separator system property. */
  val Newline = System.getProperty("line.separator")

  val utf8 = Charset.forName("UTF-8")

  private lazy val jrtFs = FileSystems.getFileSystem(URI.create("jrt:/"))

  /**
   * Returns the NIO Path to the directory, Java module, or the JAR file containing the class file `cl`.
   * If the location cannot be determined, an error is generated.
   * Note that for JDK 11 onwards, a module will return a jrt path.
   */
  def classLocationPath(cl: Class[_]): NioPath = {
    val u = classLocation(cl)
    val p = u.getProtocol match {
      case FileScheme => Option(toFile(u).toPath)
      case "jar"      => urlAsFile(u) map { _.toPath }
      case "jrt"      => Option(IO.jrtFs.getPath(u.getPath))
      case _          => None
    }
    p.getOrElse(sys.error(s"Unable to create File from $u for $cl"))
  }

  /**
   * Returns a NIO Path to the directory, Java module, or the JAR file for type `A` (as determined by an implicit Manifest).
   * If the location cannot be determined, an error is generated.
   * Note that for JDK 11 onwards, a module will return a jrt path.
   */
  def classLocationPath[A](implicit mf: SManifest[A]): NioPath =
    classLocationPath(mf.runtimeClass)

  /**
   * Returns the directory, Java module, or the JAR containing the class file `cl`.
   * If the location cannot be determined or it is not a file, an error is generated.
   * Note that for JDK 11 onwards, the returned module path cannot be expressed as `File`, so it will return `None`.
   */
  def classLocationFileOption(cl: Class[_]): Option[File] = {
    val u = classLocation(cl)
    urlAsFile(u)
  }

  /**
   * Returns the directory, Java module, or the JAR containing the class file for type `T` (as determined by an implicit Manifest).
   * If the location cannot be determined or it is not a file, an error is generated.
   * Note that for JDK 11 onwards, the returned module path cannot be expressed as `File`, so it will return `None`.
   */
  def classLocationFileOption[A](implicit mf: SManifest[A]): Option[File] =
    classLocationFileOption(mf.runtimeClass)

  /**
   * Returns the directory, Java module, or the JAR file containing the class file `cl`.
   * If the location cannot be determined or it is not a file, an error is generated.
   * Note that for JDK 11 onwards, the returned module path cannot be expressed as `File`.
   */
  @deprecated(
    "classLocationFile may not work on JDK 11. Use classfileLocation, classLocationFileOption, or classLocationPath instead.",
    "1.3.0")
  def classLocationFile(cl: Class[_]): File =
    classLocationFileOption(cl).getOrElse(sys.error(s"Unable to create File from $cl"))

  /**
   * Returns the directory, Java module, or the JAR file containing the class file for type `T` (as determined by an implicit Manifest).
   * If the location cannot be determined, an error is generated.
   * Note that for JDK 11 onwards, the returned module path cannot be expressed as `File`.
   */
  @deprecated(
    "classLocationFile may not work on JDK 11. Use classfileLocation, classLocationFileOption, or classLocationPath instead.",
    "1.3.0")
  def classLocationFile[T](implicit mf: SManifest[T]): File = classLocationFile(mf.runtimeClass)

  /**
   * Returns the URL to the directory, Java module, or the JAR file containing the class file `cl`.
   * If the location cannot be determined or it is not a file, an error is generated.
   * Note that for JDK 11 onwards, a module will return a jrt URL such as `jrt:/java.base`.
   */
  def classLocation(cl: Class[_]): URL = {
    def localcl: Option[URL] =
      Option(cl.getProtectionDomain.getCodeSource) flatMap { codeSource =>
        Option(codeSource.getLocation)
      }
    // This assumes that classes without code sources are System classes, and thus located in jars.
    // It returns a URL that looks like jar:file:/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/rt.jar!/java/lang/Integer.class
    val clsfile = s"${cl.getName.replace('.', '/')}.class"
    def syscl: Option[URL] =
      Option(ClassLoader.getSystemClassLoader) flatMap { classLoader =>
        Option(classLoader.getResource(clsfile))
      }
    try {
      localcl
        .orElse(syscl)
        .map(url =>
          url.getProtocol match {
            case "jar" =>
              val path = url.getPath
              val end = path.indexOf('!')
              new URL(
                if (end == -1) path
                else path.substring(0, end))
            case "jrt" =>
              val path = url.getPath
              val end = path.indexOf('/', 1)
              new URL("jrt",
                      null,
                      if (end == -1) path
                      else path.substring(0, end))
            case _ => url
        })
        .getOrElse(sys.error("No class location for " + cl))
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        throw e
    }
  }

  /**
   * Returns the URL to the directory, Java module, or the JAR file containing the class file `cl`.
   * If the location cannot be determined or it is not a file, an error is generated.
   * Note that for JDK 11 onwards, a module will return a jrt path.
   */
  def classLocation[A](implicit mf: SManifest[A]): URL =
    classLocation(mf.runtimeClass)

  /**
   * Returns a URL for the classfile containing the given class file for type `T` (as determined by an implicit Manifest).
   * If the location cannot be determined, an error is generated.
   */
  def classfileLocation[T](implicit mf: SManifest[T]): URL = classfileLocation(mf.runtimeClass)

  /**
   * Returns a URL for the classfile containing the given class
   * If the location cannot be determined, an error is generated.
   */
  def classfileLocation(cl: Class[_]): URL = {
    val clsfile = s"${cl.getName.replace('.', '/')}.class"
    def localcl: Option[URL] =
      Option(cl.getClassLoader) flatMap { classLoader =>
        Option(classLoader.getResource(clsfile))
      }
    def syscl: Option[URL] =
      Option(ClassLoader.getSystemClassLoader) flatMap { classLoader =>
        Option(classLoader.getResource(clsfile))
      }
    try {
      localcl
        .orElse(syscl)
        .getOrElse(sys.error("No class location for " + cl))
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        throw e
    }
  }

  /**
   * Constructs a File corresponding to `url`, which must have a scheme of `file`.
   * This method properly works around an issue with a simple conversion to URI and then to a File.
   *
   * On Windows this can accept the following patterns of URLs:
   *
   * `val u0 = new URL("file:C:\\Users\\foo/.sbt/preloaded")`,
   * `val u1 = new URL("file:/C:\\Users\\foo/.sbt/preloaded")`,
   * `val u2 = new URL("file://unc/Users/foo/.sbt/preloaded")`,
   * `val u3 = new URL("file:///C:\\Users\\foo/.sbt/preloaded")`, and
   * `val u4 = new URL("file:////unc/Users/foo/.sbt/preloaded")`.
   */
  def toFile(url: URL): File =
    try { uriToFile(url.toURI) } catch { case _: URISyntaxException => new File(url.getPath) }

  def toFile(uri: URI): File =
    try { uriToFile(uri) } catch { case _: URISyntaxException => new File(uri.getPath) }

  /** Converts the given URL to a File.  If the URL is for an entry in a jar, the File for the jar is returned. */
  def asFile(url: URL): File = urlAsFile(url) getOrElse sys.error("URL is not a file: " + url)
  def urlAsFile(url: URL): Option[File] =
    url.getProtocol match {
      case FileScheme => Some(toFile(url))
      case "jar" =>
        val path = url.getPath
        val end = path.indexOf('!')
        Some(uriToFile(if (end == -1) path else path.substring(0, end)))
      case _ => None
    }

  private[this] def uriToFile(uriString: String): File = uriToFile(new URI(uriString))

  /**
   * Converts the given file URI to a File.
   */
  private[this] def uriToFile(uri: URI): File = {
    val part = uri.getSchemeSpecificPart
    // scheme might be omitted for relative URI reference.
    assert(
      Option(uri.getScheme) match {
        case None | Some(FileScheme) => true
        case _                       => false
      },
      s"Expected protocol to be '$FileScheme' or empty in URI $uri"
    )
    Option(uri.getAuthority) match {
      case None if part startsWith "/" => new File(uri)
      case _                           =>
        // https://github.com/sbt/sbt/issues/564
        // https://github.com/sbt/sbt/issues/3086
        // http://blogs.msdn.com/b/ie/archive/2006/12/06/file-uris-in-windows.aspx
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=5086147
        // The specific problem here is that `uri` will have a defined authority component for UNC names like //foo/bar/some/path.jar
        // but the File constructor requires URIs with an undefined authority component.
        if (!(part startsWith "/") && (part contains ":")) new File("//" + part)
        else new File(part)
    }
  }

  def assertDirectory(file: File) =
    assert(
      file.isDirectory,
      (if (file.exists) "Not a directory: " else "Directory not found: ") + file
    )

  def assertDirectories(file: File*) = file.foreach(assertDirectory)

  // "base.extension" -> (base, extension)
  /**
   * Splits the given string into base and extension strings.
   * If `name` contains no period, the base string is the input string and the extension is the empty string.
   * Otherwise, the base is the substring up until the last period (exclusive) and
   * the extension is the substring after the last period.
   *
   * For example, `split("Build.scala") == ("Build", "scala")`
   */
  def split(name: String): (String, String) = {
    val lastDot = name.lastIndexOf('.')
    if (lastDot >= 0)
      (name.substring(0, lastDot), name.substring(lastDot + 1))
    else
      (name, "")
  }

  /**
   * Each input file in `files` is created if it doesn't exist.
   * If a file already exists, the last modified time is set to the current time.
   * It is not guaranteed that all files will have the same last modified time after this call.
   */
  def touch(files: Traversable[File]): Unit = files foreach (f => { touch(f); () })

  /**
   * Creates a file at the given location if it doesn't exist.
   * If the file already exists and `setModified` is true, this method sets the last modified time to the current time.
   */
  def touch(file: File, setModified: Boolean = true): Unit = {
    val absFile = file.getAbsoluteFile
    createDirectory(absFile.getParentFile)
    val created = translate("Could not create file " + absFile) { absFile.createNewFile() }
    if (created || absFile.isDirectory)
      ()
    else if (setModified && !setModifiedTimeOrFalse(absFile, System.currentTimeMillis))
      sys.error("Could not update last modified time for file " + absFile)
  }

  /** Creates directories `dirs` and all parent directories.  It tries to work around a race condition in `File.mkdirs()` by retrying up to a limit.*/
  def createDirectories(dirs: Traversable[File]): Unit =
    dirs.foreach(createDirectory)

  /** Creates directory `dir` and all parent directories.  It tries to work around a race condition in `File.mkdirs()` by retrying up to a limit.*/
  def createDirectory(dir: File): Unit = {
    def failBase = "Could not create directory " + dir
    // Need a retry because Files.createDirectories may fail before succeeding on (at least) windows.
    val path = dir.toPath
    try Retry(
      try Files.createDirectories(path)
      catch { case _: IOException if Files.isDirectory(path) => },
      excludedExceptions = classOf[FileAlreadyExistsException]
    )
    catch { case e: IOException => throw new IOException(failBase + ": " + e, e) }
    ()
  }

  /** Gzips the file 'in' and writes it to 'out'.  'in' cannot be the same file as 'out'. */
  def gzip(in: File, out: File): Unit = {
    require(in != out, "Input file cannot be the same as the output file.")
    Using.fileInputStream(in) { inputStream =>
      Using.fileOutputStream()(out) { outputStream =>
        gzip(inputStream, outputStream)
      }
    }
  }

  /** Gzips the InputStream 'in' and writes it to 'output'.  Neither stream is closed.*/
  def gzip(input: InputStream, output: OutputStream): Unit =
    gzipOutputStream(output)(gzStream => transfer(input, gzStream))

  /** Gunzips the file 'in' and writes it to 'out'.  'in' cannot be the same file as 'out'. */
  def gunzip(in: File, out: File): Unit = {
    require(in != out, "Input file cannot be the same as the output file.")
    Using.fileInputStream(in) { inputStream =>
      Using.fileOutputStream()(out) { outputStream =>
        gunzip(inputStream, outputStream)
      }
    }
  }

  /** Gunzips the InputStream 'input' and writes it to 'output'.  Neither stream is closed.*/
  def gunzip(input: InputStream, output: OutputStream): Unit =
    gzipInputStream(input)(gzStream => transfer(gzStream, output))

  def unzip(
      from: File,
      toDirectory: File,
      filter: NameFilter = AllPassFilter,
      preserveLastModified: Boolean = true
  ): Set[File] =
    fileInputStream(from)(in => unzipStream(in, toDirectory, filter, preserveLastModified))

  def unzipURL(
      from: URL,
      toDirectory: File,
      filter: NameFilter = AllPassFilter,
      preserveLastModified: Boolean = true
  ): Set[File] =
    urlInputStream(from)(in => unzipStream(in, toDirectory, filter, preserveLastModified))

  def unzipStream(
      from: InputStream,
      toDirectory: File,
      filter: NameFilter = AllPassFilter,
      preserveLastModified: Boolean = true
  ): Set[File] = {
    createDirectory(toDirectory)
    zipInputStream(from)(zipInput => extract(zipInput, toDirectory, filter, preserveLastModified))
  }

  private def extract(
      from: ZipInputStream,
      toDirectory: File,
      filter: NameFilter,
      preserveLastModified: Boolean
  ) = {
    val set = new HashSet[File]
    @tailrec def next(): Unit = {
      val entry = from.getNextEntry
      if (entry == null)
        ()
      else {
        val name = entry.getName
        if (filter.accept(name)) {
          val target = new File(toDirectory, name)
          //log.debug("Extracting zip entry '" + name + "' to '" + target + "'")
          if (entry.isDirectory)
            createDirectory(target)
          else {
            set += target
            translate("Error extracting zip entry '" + name + "' to '" + target + "': ") {
              fileOutputStream(false)(target)(out => transfer(from, out))
            }
          }
          if (preserveLastModified)
            setModifiedTimeOrFalse(target, entry.getTime)
        } else {
          //log.debug("Ignoring zip entry '" + name + "'")
        }
        from.closeEntry()
        next()
      }
    }
    next()
    Set() ++ set
  }

  // TODO: provide a better API to download things.
  // /** Retrieves the content of the given URL and writes it to the given File. */
  // def download(url: URL, to: File) =
  //   Using.urlInputStream(url) { inputStream =>
  //     transfer(inputStream, to)
  //   }

  /** Copies the contents of `in` to `out`.*/
  def transfer(in: File, out: File): Unit =
    fileInputStream(in)(in => transfer(in, out))

  /**
   * Copies the contents of the input file `in` to the `out` stream.
   * The output stream is not closed by this method.
   */
  def transfer(in: File, out: OutputStream): Unit =
    fileInputStream(in)(in => transfer(in, out))

  /** Copies all bytes from the given input stream to the given File.  The input stream is not closed by this method.*/
  def transfer(in: InputStream, to: File): Unit =
    Using.fileOutputStream()(to) { outputStream =>
      transfer(in, outputStream)
    }

  /**
   * Copies all bytes from the given input stream to the given output stream.
   * Neither stream is closed.
   */
  def transfer(in: InputStream, out: OutputStream): Unit = transferImpl(in, out, false)

  /**
   * Copies all bytes from the given input stream to the given output stream.  The
   * input stream is closed after the method completes.
   */
  def transferAndClose(in: InputStream, out: OutputStream): Unit = transferImpl(in, out, true)
  private def transferImpl(in: InputStream, out: OutputStream, close: Boolean) = {
    try {
      val buffer = new Array[Byte](BufferSize)
      @tailrec def read(): Unit = {
        val byteCount = in.read(buffer)
        if (byteCount >= 0) {
          out.write(buffer, 0, byteCount)
          read()
        }
      }
      read()
    } finally { if (close) in.close }
  }

  /**
   * Creates a temporary directory and provides its location to the given function.  The directory
   * is deleted after the function returns if `keepDirectory` is set to false.
   */
  def withTemporaryDirectory[T](action: File => T, keepDirectory: Boolean): T = {
    val dir = createTemporaryDirectory
    try { action(dir) } finally { if (!keepDirectory) delete(dir) }
  }

  /**
   * Overload of `withTemporaryDirectory` with `keepDirectory` set to false.
   */
  def withTemporaryDirectory[T](action: File => T): T =
    withTemporaryDirectory(action, keepDirectory = false)

  /** Creates a directory in the default temporary directory with a name generated from a random integer. */
  def createTemporaryDirectory: File = createUniqueDirectory(temporaryDirectory)

  /** Creates a directory in `baseDirectory` with a name generated from a random integer */
  def createUniqueDirectory(baseDirectory: File): File = {
    def create(tries: Int): File = {
      if (tries > MaximumTries)
        sys.error("Could not create temporary directory.")
      else {
        val randomName = "sbt_" + java.lang.Integer.toHexString(random.nextInt)
        val f = new File(baseDirectory, randomName)

        try { createDirectory(f); f } catch { case NonFatal(_) => create(tries + 1) }
      }
    }
    create(0)
  }

  /**
   * Creates a file in the default temporary directory, calls `action` with the
   * file, deletes the file if `keepFile` is set to true, and returns the
   * result of calling `action`. The name of the file will begin with `prefix`,
   * which must be at least three characters long, and end with `postfix`, which
   * has no minimum length.
   */
  def withTemporaryFile[T](prefix: String, postfix: String, keepFile: Boolean)(
      action: File => T
  ): T = {
    val file = File.createTempFile(prefix, postfix)
    try { action(file) } finally { if (!keepFile) file.delete(); () }
  }

  /**
   * Overload of `withTemporaryFile` with `keepFile` set to false.
   */
  def withTemporaryFile[T](prefix: String, postfix: String)(action: File => T): T =
    withTemporaryFile(prefix, postfix, keepFile = false)(action)

  private[sbt] def jars(dir: File): Iterable[File] = listFiles(dir, GlobFilter("*.jar"))

  /** Deletes all empty directories in the set.  Any non-empty directories are ignored. */
  def deleteIfEmpty(dirs: collection.Set[File]): Unit = {
    val isEmpty = new HashMap[File, Boolean]
    def visit(f: File): Boolean =
      isEmpty.getOrElseUpdate(f, dirs(f) && f.isDirectory && (f.listFiles forall visit))

    dirs foreach visit
    for ((f, true) <- isEmpty) f.delete
  }

  /** Deletes each file or directory (recursively) in `files`.*/
  def delete(files: Iterable[File]): Unit = files.foreach(delete)

  /** Deletes each file or directory in `files` recursively.  Any empty parent directories are deleted, recursively.*/
  def deleteFilesEmptyDirs(files: Iterable[File]): Unit = {
    def isEmptyDirectory(dir: File) = dir.isDirectory && listFiles(dir).isEmpty
    def parents(fs: Set[File]) = fs flatMap (f => Option(f.getParentFile))
    @tailrec def deleteEmpty(dirs: Set[File]): Unit = {
      val empty = dirs filter isEmptyDirectory
      if (empty.nonEmpty) // looks funny, but this is true if at least one of `dirs` is an empty directory
        {
          empty foreach { _.delete() }
          deleteEmpty(parents(empty))
        }
    }

    delete(files)
    deleteEmpty(parents(files.toSet))
  }

  /** Deletes `file`, recursively if it is a directory. */
  def delete(file: File): Unit = Retry {
    try {
      FileTreeView.DEFAULT_NIO.list(Glob(file.toPath, (1, 1), AllPass), _ => true).foreach {
        case (dir, attrs) if attrs.isDirectory => delete(dir.toFile)
        case (f, _)                            => Files.deleteIfExists(f)
      }
    } catch {
      case _: NotDirectoryException =>
    }
    file.delete()
    ()
  }

  /** Returns the children of directory `dir` that match `filter` in a non-null array.*/
  def listFiles(filter: java.io.FileFilter)(dir: File): Array[File] =
    wrapNull(dir.listFiles(filter))

  /** Returns the children of directory `dir` that match `filter` in a non-null array.*/
  def listFiles(dir: File, filter: java.io.FileFilter): Array[File] =
    wrapNull(dir.listFiles(filter))

  /** Returns the children of directory `dir` in a non-null array.*/
  def listFiles(dir: File): Array[File] = wrapNull(dir.listFiles())

  private[sbt] def wrapNull(a: Array[File]) = if (a == null) new Array[File](0) else a

  /**
   * Creates a jar file.
   *
   * @param sources The files to include in the jar file paired with the entry name in the jar.
   *                Only the pairs explicitly listed are included.
   * @param outputJar The file to write the jar to.
   * @param manifest The manifest for the jar.
   */
  def jar(sources: Traversable[(File, String)], outputJar: File, manifest: Manifest): Unit =
    archive(sources.toSeq, outputJar, Some(manifest))

  /**
   * Creates a zip file.
   * @param sources The files to include in the zip file paired with the entry name in the zip.
   *                Only the pairs explicitly listed are included.
   * @param outputZip The file to write the zip to.
   */
  def zip(sources: Traversable[(File, String)], outputZip: File): Unit =
    archive(sources.toSeq, outputZip, None)

  private def archive(
      sources: Seq[(File, String)],
      outputFile: File,
      manifest: Option[Manifest]
  ) = {
    if (outputFile.isDirectory)
      sys.error("Specified output file " + outputFile + " is a directory.")
    else {
      val outputDir = outputFile.getParentFile match {
        case null       => new File(".")
        case parentFile => parentFile
      }
      createDirectory(outputDir)
      withZipOutput(outputFile, manifest) { output =>
        val createEntry: (String => ZipEntry) =
          if (manifest.isDefined) new JarEntry(_) else new ZipEntry(_)
        writeZip(sources, output)(createEntry)
      }
    }
  }
  private def writeZip(sources: Seq[(File, String)], output: ZipOutputStream)(
      createEntry: String => ZipEntry
  ) = {
    val files = sources.flatMap {
      case (file, name) => if (file.isFile) (file, normalizeName(name)) :: Nil else Nil
    }

    val now = System.currentTimeMillis
    // The CRC32 for an empty value, needed to store directories in zip files
    val emptyCRC = new CRC32().getValue()

    def addDirectoryEntry(name: String) = {
      output putNextEntry makeDirectoryEntry(name)
      output.closeEntry()
    }

    def makeDirectoryEntry(name: String) = {
      //			log.debug("\tAdding directory " + relativePath + " ...")
      val e = createEntry(name)
      e setTime now
      e setSize 0
      e setMethod ZipEntry.STORED
      e setCrc emptyCRC
      e
    }

    def makeFileEntry(file: File, name: String) = {
      //			log.debug("\tAdding " + file + " as " + name + " ...")
      val e = createEntry(name)
      e setTime getModifiedTimeOrZero(file)
      e
    }
    def addFileEntry(file: File, name: String) = {
      output putNextEntry makeFileEntry(file, name)
      transfer(file, output)
      output.closeEntry()
    }

    //Calculate directories and add them to the generated Zip
    allDirectoryPaths(files) foreach addDirectoryEntry

    //Add all files to the generated Zip
    files foreach { case (file, name) => addFileEntry(file, name) }
  }

  // map a path a/b/c to List("a", "b")
  private def relativeComponents(path: String): List[String] =
    path.split("/").toList.dropRight(1)

  // map components List("a", "b", "c") to List("a/b/c/", "a/b/", "a/", "")
  private def directories(path: List[String]): List[String] =
    path.foldLeft(List(""))((e, l) => (e.head + l + "/") :: e)

  // map a path a/b/c to List("a/b/", "a/")
  private def directoryPaths(path: String): List[String] =
    directories(relativeComponents(path)).filter(_.length > 1)

  // produce a sorted list of all the subdirectories of all provided files
  private def allDirectoryPaths(files: Iterable[(File, String)]) =
    TreeSet[String]() ++ (files flatMap { case (_, name) => directoryPaths(name) })

  private def normalizeName(name: String) = {
    val sep = File.separatorChar
    if (sep == '/') name else name.replace(sep, '/')
  }

  private def withZipOutput(file: File, manifest: Option[Manifest])(f: ZipOutputStream => Unit) = {
    fileOutputStream(false)(file) { fileOut =>
      val (zipOut, _) =
        manifest match {
          case Some(mf) =>
            import Attributes.Name.MANIFEST_VERSION
            val main = mf.getMainAttributes
            if (!main.containsKey(MANIFEST_VERSION))
              main.put(MANIFEST_VERSION, "1.0")
            (new JarOutputStream(fileOut, mf), "jar")
          case None => (new ZipOutputStream(fileOut), "zip")
        }
      try { f(zipOut) } finally { zipOut.close }
    }
  }

  /**
   * Returns the relative file for `file` relative to directory `base` or None if `base` is not a parent of `file`.
   * If `file` or `base` are not absolute, they are first resolved against the current working directory.
   */
  def relativizeFile(base: File, file: File): Option[File] =
    relativize(base, file).map(path => new File(path))

  /**
   * Returns the path for `file` relative to directory `base` or None if `base` is not a parent of `file`.
   * If `file` or `base` are not absolute, they are first resolved against the current working directory.
   */
  def relativize(base: File, file: File): Option[String] = {
    // "On UNIX systems, a pathname is absolute if its prefix is "/"."
    // https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#isAbsolute
    // "This typically involves removing redundant names such as "." and ".." from the pathname, resolving symbolic links (on UNIX platforms)"
    // https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#getCanonicalPath()
    // We do not want to use getCanonicalPath because if we resolve the symbolic link, that could change
    // the outcome of copyDirectory's target structure.
    // Path#normailize is able to expand ".." without expanding the symlink.
    // https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#normalize()
    // "Returns a path that is this path with redundant name elements eliminated."
    def toAbsolutePath(x: File): NioPath = {
      val p = x.toPath
      if (!p.isAbsolute) p.toAbsolutePath
      else p
    }
    val basePath = toAbsolutePath(base).normalize
    val filePath = toAbsolutePath(file).normalize
    if (filePath startsWith basePath) {
      val relativePath = catching(classOf[IllegalArgumentException]) opt (basePath relativize filePath)
      relativePath map (_.toString)
    } else None
  }

  def copy(sources: Traversable[(File, File)]): Set[File] = copy(sources, CopyOptions())

  /**
   * For each pair in `sources`, copies the contents of the first File (the source) to the location
   * of the second File (the target).
   *
   * See [[sbt.io.CopyOptions]] for docs on the options available.
   *
   * Any parent directories that do not exist are created.
   * The set of all target files is returned, whether or not they were updated by this method.
   */
  def copy(sources: Traversable[(File, File)], options: CopyOptions): Set[File] =
    copy(sources, options.overwrite, options.preserveLastModified, options.preserveExecutable)

  def copy(
      sources: Traversable[(File, File)],
      overwrite: Boolean,
      preserveLastModified: Boolean,
      preserveExecutable: Boolean
  ): Set[File] =
    sources.map(tupled(copyImpl(overwrite, preserveLastModified, preserveExecutable))).toSet

  private def copyImpl(
      overwrite: Boolean,
      preserveLastModified: Boolean,
      preserveExecutable: Boolean
  )(from: File, to: File): File = {
    if (overwrite || !to.exists || getModifiedTimeOrZero(from) > getModifiedTimeOrZero(to)) {
      if (from.isDirectory)
        createDirectory(to)
      else {
        createDirectory(to.getParentFile)
        copyFile(from, to, preserveLastModified, preserveExecutable)
      }
    }
    to
  }

  def copyDirectory(source: File, target: File): Unit = copyDirectory(source, target, CopyOptions())

  /**
   * Copies the contents of each file in the `source` directory to the corresponding file in the
   * `target` directory.
   *
   * See [[sbt.io.CopyOptions]] for docs on the options available.
   *
   * Files in `target` without a corresponding file in `source` are left unmodified in any case.
   * Any parent directories that do not exist are created.
   */
  def copyDirectory(source: File, target: File, options: CopyOptions): Unit =
    copyDirectory(
      source,
      target,
      options.overwrite,
      options.preserveLastModified,
      options.preserveExecutable
    )

  def copyDirectory(
      source: File,
      target: File,
      overwrite: Boolean = false,
      preserveLastModified: Boolean = false,
      preserveExecutable: Boolean = true
  ): Unit = {
    val sources = PathFinder(source).allPaths pair Path.rebase(source, target)
    copy(sources, overwrite, preserveLastModified, preserveExecutable)
    ()
  }

  def copyFile(sourceFile: File, targetFile: File): Unit =
    copyFile(sourceFile, targetFile, CopyOptions())

  /**
   * Copies the contents of `sourceFile` to the location of `targetFile`, overwriting any existing content.
   *
   * See [[sbt.io.CopyOptions]] for docs on the options available.
   */
  def copyFile(sourceFile: File, targetFile: File, options: CopyOptions): Unit =
    copyFile(sourceFile, targetFile, options.preserveLastModified, options.preserveExecutable)

  def copyFile(
      sourceFile: File,
      targetFile: File,
      preserveLastModified: Boolean = false,
      preserveExecutable: Boolean = true
  ): Unit = {
    // NOTE: when modifying this code, test with larger values of CopySpec.MaxFileSizeBits than default

    require(sourceFile.exists, "Source file '" + sourceFile.getAbsolutePath + "' does not exist.")
    require(
      !sourceFile.isDirectory,
      "Source file '" + sourceFile.getAbsolutePath + "' is a directory."
    )
    fileInputChannel(sourceFile) { in =>
      fileOutputChannel(targetFile) { out =>
        // maximum bytes per transfer according to  from http://dzone.com/snippets/java-filecopy-using-nio
        val max = (64L * 1024 * 1024) - (32 * 1024)
        val total = in.size
        def loop(offset: Long): Long =
          if (offset < total)
            loop(offset + out.transferFrom(in, offset, max))
          else
            offset
        val copied = loop(0)
        if (copied != in.size)
          sys.error(
            "Could not copy '" + sourceFile + "' to '" + targetFile + "' (" + copied + "/" + in.size + " bytes copied)"
          )
      }
    }
    if (preserveLastModified) {
      copyLastModified(sourceFile, targetFile)
      ()
    }
    if (preserveExecutable) {
      copyExecutable(sourceFile, targetFile)
      ()
    }
  }

  /** Transfers the executable property of `sourceFile` to `targetFile`. */
  def copyExecutable(sourceFile: File, targetFile: File) = {
    val executable = sourceFile.canExecute
    if (executable) targetFile.setExecutable(true)
  }

  /** The default Charset used when not specified: UTF-8. */
  def defaultCharset = utf8

  /**
   * Writes `content` to `file` using `charset` or UTF-8 if `charset` is not explicitly specified.
   * If `append` is `false`, the existing contents of `file` are overwritten.
   * If `append` is `true`, the new `content` is appended to the existing contents.
   * If `file` or any parent directories do not exist, they are created.
   */
  def write(
      file: File,
      content: String,
      charset: Charset = defaultCharset,
      append: Boolean = false
  ): Unit =
    writer(file, content, charset, append) { _.write(content) }

  def writer[T](file: File, content: String, charset: Charset, append: Boolean = false)(
      f: BufferedWriter => T
  ): T =
    if (charset.newEncoder.canEncode(content))
      fileWriter(charset, append)(file)(f)
    else
      sys.error("String cannot be encoded by charset " + charset.name)

  def reader[T](file: File, charset: Charset = defaultCharset)(f: BufferedReader => T): T =
    fileReader(charset)(file) { f }

  /** Reads the full contents of `file` into a String using `charset` or UTF-8 if `charset` is not explicitly specified. */
  def read(file: File, charset: Charset = defaultCharset): String = {
    val out = new ByteArrayOutputStream(file.length.toInt)
    transfer(file, out)
    out.toString(charset.name)
  }

  /** Reads the full contents of `in` into a byte array.  This method does not close `in`.*/
  def readStream(in: InputStream, charset: Charset = defaultCharset): String = {
    val out = new ByteArrayOutputStream
    transfer(in, out)
    out.toString(charset.name)
  }

  /** Reads the full contents of `in` into a byte array. */
  def readBytes(file: File): Array[Byte] = fileInputStream(file)(readBytes)

  /** Reads the full contents of `in` into a byte array.  This method does not close `in`. */
  def readBytes(in: InputStream): Array[Byte] = {
    val out = new ByteArrayOutputStream
    transfer(in, out)
    out.toByteArray
  }

  /**
   * Appends `content` to the existing contents of `file` using `charset` or UTF-8 if `charset` is not explicitly specified.
   * If `file` does not exist, it is created, as are any parent directories.
   */
  def append(file: File, content: String, charset: Charset = defaultCharset): Unit =
    write(file, content, charset, true)

  /**
   * Appends `bytes` to the existing contents of `file`.
   * If `file` does not exist, it is created, as are any parent directories.
   */
  def append(file: File, bytes: Array[Byte]): Unit =
    writeBytes(file, bytes, true)

  /**
   * Writes `bytes` to `file`, overwriting any existing content.
   * If any parent directories do not exist, they are first created.
   */
  def write(file: File, bytes: Array[Byte]): Unit =
    writeBytes(file, bytes, false)

  private def writeBytes(file: File, bytes: Array[Byte], append: Boolean): Unit =
    fileOutputStream(append)(file) { _.write(bytes) }

  /** Reads all of the lines from `url` using the provided `charset` or UTF-8 if `charset` is not explicitly specified. */
  def readLinesURL(url: URL, charset: Charset = defaultCharset): List[String] =
    urlReader(charset)(url)(readLines)

  /** Reads all of the lines in `file` using the provided `charset` or UTF-8 if `charset` is not explicitly specified. */
  def readLines(file: File, charset: Charset = defaultCharset): List[String] =
    fileReader(charset)(file)(readLines)

  /** Reads all of the lines from `in`.  This method does not close `in`.*/
  def readLines(in: BufferedReader): List[String] =
    foldLines[List[String]](in, Nil)((accum, line) => line :: accum).reverse

  /** Applies `f` to each line read from `in`. This method does not close `in`.*/
  def foreachLine(in: BufferedReader)(f: String => Unit): Unit =
    foldLines(in, ())((_, line) => f(line))

  /**
   * Applies `f` to each line read from `in` and the accumulated value of type `T`, with initial value `init`.
   * This method does not close `in`.
   */
  def foldLines[T](in: BufferedReader, init: T)(f: (T, String) => T): T = {
    def readLine(accum: T): T = {
      val line = in.readLine()
      if (line eq null) accum else readLine(f(accum, line))
    }
    readLine(init)
  }

  /**
   * Writes `lines` to `file` using the given `charset` or UTF-8 if `charset` is not explicitly specified.
   * If `append` is `false`, the contents of the file are overwritten.
   * If `append` is `true`, the lines are appended to the file.
   * A newline is written after each line and NOT before the first line.
   * If any parent directories of `file` do not exist, they are first created.
   */
  def writeLines(
      file: File,
      lines: Seq[String],
      charset: Charset = defaultCharset,
      append: Boolean = false
  ): Unit =
    writer(file, lines.headOption.getOrElse(""), charset, append) { w =>
      lines.foreach(line => { w.write(line); w.newLine() })
    }

  /** Writes `lines` to `writer` using `writer`'s `println` method. */
  def writeLines(writer: PrintWriter, lines: Seq[String]): Unit =
    lines foreach writer.println

  /**
   * Writes `properties` to the File `to`, using `label` as the comment on the first line.
   * If any parent directories of `to` do not exist, they are first created.
   */
  def write(properties: Properties, label: String, to: File) =
    fileOutputStream()(to)(output => properties.store(output, label))

  /** Reads the properties in `from` into `properties`.  If `from` does not exist, `properties` is left unchanged.*/
  def load(properties: Properties, from: File): Unit =
    if (from.exists)
      fileInputStream(from)(input => properties.load(input))

  /** A pattern used to split a String by path separator characters.*/
  private val PathSeparatorPattern = java.util.regex.Pattern.compile(File.pathSeparator)

  /** Splits a String around the platform's path separator characters. */
  def pathSplit(s: String) = PathSeparatorPattern.split(s)

  /**
   * Move the provided files to a temporary location.
   *   If 'f' returns normally, delete the files.
   *   If 'f' throws an Exception, return the files to their original location.
   */
  def stash[T](files: Set[File])(f: => T): T =
    withTemporaryDirectory { dir =>
      val stashed = stashLocations(dir, files.toArray)
      move(stashed)

      try { f } catch {
        case e: Exception =>
          try { move(stashed.map(_.swap)); throw e } catch { case _: Exception => throw e }
      }
    }

  private def stashLocations(dir: File, files: Array[File]) =
    for ((file, index) <- files.zipWithIndex) yield (file, new File(dir, index.toHexString))

  // TODO: the reference to the other move overload does not resolve, probably due to a scaladoc bug
  /**
   * For each pair in `files`, moves the contents of the first File to the location of the second.
   * See `sbt.io.IO$.move(java.io.File,java.io.File):Unit` for the behavior of the individual move operations.
   */
  def move(files: Traversable[(File, File)]): Unit =
    files.foreach(Function.tupled(move))

  /**
   * Moves the contents of `a` to the location specified by `b`.
   * This method deletes any content already at `b` and creates any parent directories of `b` if they do not exist.
   * It will first try `File.renameTo` and if that fails, resort to copying and then deleting the original file.
   * In either case, the original File will not exist on successful completion of this method.
   */
  def move(a: File, b: File): Unit = {
    if (b.exists)
      delete(b)
    createDirectory(b.getParentFile)
    if (!a.renameTo(b)) {
      copyFile(a, b, true)
      delete(a)
    }
  }

  /**
   * Applies `f` to a buffered gzip `OutputStream` for `file`.
   * The streams involved are opened before calling `f` and closed after it returns.
   * The result is the result of `f`.
   */
  def gzipFileOut[T](file: File)(f: OutputStream => T): T =
    Using.fileOutputStream()(file) { fout =>
      Using.gzipOutputStream(fout) { outg =>
        Using.bufferedOutputStream(outg)(f)
      }
    }

  /**
   * Applies `f` to a buffered gzip `InputStream` for `file`.
   * The streams involved are opened before calling `f` and closed after it returns.
   * The result is the result of `f`.
   */
  def gzipFileIn[T](file: File)(f: InputStream => T): T =
    Using.fileInputStream(file) { fin =>
      Using.gzipInputStream(fin) { ing =>
        Using.bufferedInputStream(ing)(f)
      }
    }

  /**
   * Converts an absolute File to a URI.  The File is converted to a URI (toURI),
   * normalized (normalize), encoded (toASCIIString), and a forward slash ('/') is appended to the path component if
   * it does not already end with a slash.
   */
  def directoryURI(dir: File): URI = {
    assertAbsolute(dir)
    directoryURI(dir.toURI.normalize)
  }

  /**
   * Converts an absolute File to a URI.  The File is converted to a URI (toURI),
   * normalized (normalize), encoded (toASCIIString), and a forward slash ('/') is appended to the path component if
   * it does not already end with a slash.
   */
  def directoryURI(uri: URI): URI = {
    if (!uri.isAbsolute) return uri; //assertAbsolute(uri)
    val str = uri.toASCIIString
    val dirURI =
      if (str.endsWith("/") || uri.getScheme != FileScheme || (uri.getRawFragment ne null))
        uri
      else
        new URI(str + "/")

    dirURI.normalize
  }

  /** Converts the given File to a URI.  If the File is relative, the URI is relative, unlike File.toURI*/
  def toURI(f: File): URI =
    if (f.isAbsolute) {
      f.toPath.toUri
    } else {
      // need to use the three argument URI constructor because the single argument version doesn't encode
      new URI(null, normalizeName(f.getPath), null)
    }

  /**
   * Resolves `f` against `base`, which must be an absolute directory.
   * The result is guaranteed to be absolute.
   * If `f` is absolute, it is returned without changes.
   */
  def resolve(base: File, f: File): File = {
    assertAbsolute(base)
    val fabs = if (f.isAbsolute) f else new File(directoryURI(new File(base, f.getPath)))
    assertAbsolute(fabs)
    fabs
  }
  def assertAbsolute(f: File) = assert(f.isAbsolute, "Not absolute: " + f)
  def assertAbsolute(uri: URI) = assert(uri.isAbsolute, "Not absolute: " + uri)

  /** Parses a classpath String into File entries according to the current platform's path separator.*/
  def parseClasspath(s: String): Seq[File] = IO.pathSplit(s).map(new File(_)).toSeq

  /**
   * Constructs an `ObjectInputStream` on `wrapped` that uses `loader` to load classes.
   * See also [[https://github.com/sbt/sbt/issues/136 issue 136]].
   */
  def objectInputStream(wrapped: InputStream, loader: ClassLoader): ObjectInputStream =
    new ObjectInputStream(wrapped) {
      override def resolveClass(osc: ObjectStreamClass): Class[_] = {
        val c = Class.forName(osc.getName, false, loader)
        if (c eq null) super.resolveClass(osc) else c
      }
    }

  /** Returns `true` if the filesystem supports POSIX file attribute view. */
  def isPosix: Boolean = hasPosixFileAttributeView

  /** Returns `true` if the filesystem supports POSIX file attribute view. */
  lazy val hasPosixFileAttributeView: Boolean = supportedFileAttributeViews.contains("posix")

  /** Returns `true` if the filesystem supports file owner attribute view. */
  lazy val hasFileOwnerAttributeView: Boolean = supportedFileAttributeViews.contains("owner")

  /** Returns `true` if the filesystem supports DOS file attribute view. */
  lazy val hasDosFileAttributeView: Boolean = supportedFileAttributeViews.contains("dos")

  /** Returns `true` if the filesystem supports ACL file attribute view. */
  lazy val hasAclFileAttributeView: Boolean = supportedFileAttributeViews.contains("acl")

  /** Returns `true` if the filesystem supports basic file attribute view. */
  lazy val hasBasicFileAttributeView: Boolean = supportedFileAttributeViews.contains("basic")

  /** Returns `true` if the filesystem supports user-defined file attribute view. */
  lazy val hasUserDefinedFileAttributeView: Boolean = supportedFileAttributeViews.contains("user")

  private[this] lazy val supportedFileAttributeViews: Set[String] = {
    FileSystems.getDefault.supportedFileAttributeViews.asScala.toSet
  }

  /**
   * Updates permission of this file.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   *
   * @param file
   * @param permissions Must be 9 character POSIX permission representation e.g. "rwxr-x---"
   */
  def setPermissions(file: File, permissions: String): Unit = {
    Path(file).setPermissions(PosixFilePermissions.fromString(permissions).asScala.toSet)
  }

  /**
   * An alias for `setPermissions`. Updates permission of this file.
   * This operation requires underlying filesystem to support `IO.isPosix`.
   *
   * @param permissions Must be 9 character POSIX permission representation e.g. "rwxr-x---"
   * @param file
   */
  def chmod(permissions: String, file: File): Unit = setPermissions(file, permissions)

  /**
   * Updates the file owner.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   *
   * @param file
   * @param owner
   */
  def setOwner(file: File, owner: String): Unit = Path(file).setOwner(owner)

  /**
   * An alias for `setOwner`. Updates the file owner.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   *
   * @param owner
   * @param file
   */
  def chown(owner: String, file: File): Unit = setOwner(file, owner)

  /**
   * Updates the group owner of the file.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   *
   * @param file
   * @param group
   */
  def setGroup(file: File, group: String): Unit = Path(file).setGroup(group)

  /**
   * An alias for setGroup. Updates the group owner of the file.
   * This operation requires underlying filesystem to support `IO.hasFileOwnerAttributeView`.
   *
   * @param group
   * @param file
   */
  def chgrp(group: String, file: File): Unit = setGroup(file, group)

  /**
   * Return the last modification timestamp of the specified file,
   * in milliseconds since the Unix epoch (January 1, 1970 UTC).
   * This method will use whenever possible native code in order to
   * return a timestamp with a 1-millisecond precision.
   *
   * This is in contrast to lastModified() in java.io.File, and to
   * getLastModifiedTime() in java.nio.file.Files, which on many implementations
   * of the JDK prior to JDK 10 will return timestamps with 1-second precision.
   *
   * If native code support is not available for the JDK/OS in use, this
   * method will revert to the Java calls. Currently supported systems
   * are Linux 64/32 bits, Windows, and OSX, all on Intel hardware.
   *
   * Please note that even on those platforms, not all filesystems
   * support sub-second timestamp resolutions. For instance, ext2/3,
   * FAT, and HFS+ all have a one second resolution or higher for
   * modification times. Conversely, ext4, NTFS, and APFS all support
   * at least millisecond resolution, or finer.
   *
   * If the file does not exist, or if it impossible to obtain the
   * modification time because of access permissions of other reasons,
   * this method will throw a FileNotFoundException or an IOException,
   * as appropriate. This is the same behavior as the nio code in
   * Files.getLastModifiedTime(). However note that, in contrast,
   * Java's traditional lastModified() in java.io.File will return
   * zero if an error occurs.
   *
   * If you do not wish to use native calls, please define the property
   * "sbt.io.jdktimestamps" to "true" (or anything other than "false"),
   * and Java's get/setLastModifiedTime() will be used instead. This
   * setting applies to setModifiedTime() and copyModifiedTime() as well.
   *
   * This method was added in sbt/io v1.1.2, but it may be
   * replaced in the future by a similar method that returns
   * a Try, rather than throwing an exception.
   * It is therefore marked as deprecated, since we cannot
   * guarantee that it will remain available in future versions.
   *
   * @see setModifiedTime
   * @see copyModifiedTime
   */
  @deprecated("This method might be removed in the future, also see getModifiedOrZero()", "1.1.3")
  def getModifiedTime(file: File): Long = Milli.getModifiedTime(file)

  /**
   * Sets the modification time of the file argument, in milliseconds
   * since the Unix epoch (January 1, 1970 UTC).
   * This method will use native code whenever possible in order to
   * achieve a subsecond precision. Please see getModifiedTime() for
   * further information.
   * If it is impossible to set the modification time, the code will
   * throw a FileNotFoundException or an IOException, as appropriate.
   * This is similar to Files.setLastModifiedTime(). Note that, in
   * contrast, Java's traditional setLastModified() will return a
   * boolean false value if an error occurs.
   *
   * This method may not work correctly if mtime is negative.
   *
   * This method was added in sbt/io v1.1.2, but it may be
   * replaced in the future by a similar method that returns
   * a Try, rather than throwing an exception.
   * It is therefore marked as deprecated, since we cannot
   * guarantee that it will remain available in future versions.
   *
   * @see getModifiedTime
   * @see copyModifiedTime
   */
  @deprecated("This method might be removed in the future, also see setModifiedTimeOrFalse()",
              "1.1.3")
  def setModifiedTime(file: File, mtime: Long): Unit = Milli.setModifiedTime(file, mtime)

  /**
   * Copies the last modification time of `fromFile` to `toFile`, with
   * a highest precision possible, by using native code when available.
   *
   * This method copies the timestamps with the highest possible precision
   * offered by the native calls of this system for the filesystem in use.
   * That could be in the region of nanoseconds. It is therefore more
   * precise than using separate getModifiedTime()/setModifiedTime() calls,
   * which will round timestamps to whole milliseconds.
   *
   * If the timestamp cannot be copied, this method will throw
   * a FileNotFoundException or an IOException, as appropriate.
   *
   * This method was added in sbt/io v1.1.2, but it may be
   * replaced in the future by a similar method that returns
   * a Try, rather than throwing an exception.
   * It is therefore marked as deprecated, since we cannot
   * guarantee that it will remain available in future versions.
   *
   * @see getModifiedTime
   * @see setModifiedTime
   */
  @deprecated("This method might be removed in the future, also see copyLastModified()", "1.1.3")
  def copyModifiedTime(fromFile: File, toFile: File): Unit =
    Milli.copyModifiedTime(fromFile, toFile)

  /**
   * Return the last modification timestamp of the specified file,
   * in milliseconds since the Unix epoch (January 1, 1970 UTC).
   *
   * If the specified file does not exist, this method will return 0L.
   *
   * The deprecated method getModifiedTime() has similar semantics,
   * but will throw an exception if the file does not exist.
   * Please refer to its documentation for additional details.
   *
   * @see getModifiedTime
   */
  def getModifiedTimeOrZero(file: File): Long =
    try {
      Retry(Milli.getModifiedTime(file), classOf[FileNotFoundException])
    } catch {
      case _: FileNotFoundException => 0L
    }

  /**
   * Sets the modification time of the file argument, in milliseconds
   * since the Unix epoch (January 1, 1970 UTC).
   *
   * If the specified file does not exist, this method will return false.
   * It will return true if the file modification time was successfully changed.
   *
   * The deprecated method setModifiedTime() has similar semantics,
   * but will throw an exception if the file does not exist.
   * Please refer to its documentation for additional details.
   *
   * This method may not work correctly if mtime is negative.
   *
   * @see setModifiedTime
   */
  def setModifiedTimeOrFalse(file: File, mtime: Long): Boolean =
    try {
      Retry(Milli.setModifiedTime(file, mtime), classOf[FileNotFoundException])
      true
    } catch {
      case _: FileNotFoundException => false
    }

  /**
   * Transfers the last modified time of `sourceFile` to `targetFile`.
   *
   * Note: this method has a special semantics if files are missing.
   * In particular, if the source file is missing, it will silently
   * set the target modification time to 1st January 1970, which
   * corresponds to the Unix epoch.
   *
   * The method returns true if the target file modification time was
   * successfully changed, false otherwise.
   *
   * The deprecated related method copyModifiedTime() has a somewhat different
   * semantics, please refer to its documentation for additional details.
   *
   * @see copyModifiedTime
   */
  def copyLastModified(sourceFile: File, targetFile: File): Boolean = {
    val last = getModifiedTimeOrZero(sourceFile)
    // getModifiedTimeOrZero can return a negative number, but setLastModified
    // (which may be used by setModifiedTimeOrFalse) doesn't accept it,
    // see Java bug #6791812
    setModifiedTimeOrFalse(targetFile, math.max(last, 0L))
  }
}
