package sbt.io

import scala.util.Try
import org.scalacheck._, Prop._
import java.nio.file.Files

object IOSpecification extends Properties("IO") {
  property("IO.classLocationPath able to determine containing directories") = forAll(classes) {
    (c: Class[_]) =>
      Try(IO.classLocationPath(c)).toOption.exists {
        case jar if jar.getFileName.toString.endsWith(".jar") =>
          Files.isRegularFile(jar)
        case jrt if jrt.toUri.getScheme == "jrt" =>
          jrt.toString.contains("/java.base")
        case f =>
          f.endsWith(c.getName.replaceAll("\\.", "/") + ".class")
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
