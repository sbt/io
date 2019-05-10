package sbt.nio

import java.nio.file.FileSystems

import scala.collection.JavaConverters._

object TestHelpers {
  val root = FileSystems.getDefault.getRootDirectories.asScala.head
  val basePath = root.resolve("foo").resolve("bar")
}
