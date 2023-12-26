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

import scala.util.Try
import org.scalacheck._, Prop._
import java.nio.file.Files

object IOSpecification extends Properties("IO") {
  property("IO.classLocationPath able to determine containing directories") = forAll(classes) {
    (c: Class[?]) =>
      Try(IO.classLocationPath(c)).toOption.exists {
        case jar if jar.getFileName.toString.endsWith(".jar") =>
          Files.isRegularFile(jar)
        case jrt if jrt.getFileSystem.provider.getScheme == "jrt" =>
          jrt.toString.contains("/java.base")
        case dir =>
          Files.isDirectory(dir)
      }
  }

  implicit def classes: Gen[Class[?]] =
    Gen.oneOf(
      this.getClass,
      classOf[java.lang.Integer],
      classOf[java.util.AbstractMap.SimpleEntry[String, String]],
      classOf[String],
      classOf[Thread],
      classOf[Properties]
    )
}
