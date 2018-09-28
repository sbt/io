package sbt.io

import scala.util.Try
import org.scalacheck._, Prop._
import java.net.URI
import java.nio.file.{ FileSystems, Files }

object IOSpecification extends Properties("IO") {
  private lazy val jrtFs = FileSystems.getFileSystem(URI.create("jrt:/"))

  property("IO.classLocationPath able to determine containing directories") = forAll(classes) {
    (c: Class[_]) =>
      Try(IO.classLocationPath(c)).toOption.exists {
        case jar if jar.getFileName.toString.endsWith(".jar") =>
          Files.isRegularFile(jar)
        case jrt if jrt.getFileSystem == jrtFs =>
          jrt.toString.contains("/java.base")
        case dir =>
          Files.isDirectory(dir)
      }
  }

  implicit def classes: Gen[Class[_]] =
    Gen.oneOf(
      this.getClass,
      classOf[java.lang.Integer],
      classOf[java.util.AbstractMap.SimpleEntry[String, String]],
      classOf[String],
      classOf[Thread],
      classOf[Properties]
    )
}
