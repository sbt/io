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

import scala.util.Try
import org.scalacheck._, Prop._
import java.nio.file.Files

object IOSpecification extends Properties("IO") {
  property("IO.classLocationPath able to determine containing directories") = forAll(classes) {
    (c: Class[_]) =>
      Try(IO.classLocationPath(c)).toOption.exists {
        case jar if jar.getFileName.toString.endsWith(".jar") =>
          Files.isRegularFile(jar)
        case jrt if jrt.getFileSystem.provider.getScheme == "jrt" =>
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
