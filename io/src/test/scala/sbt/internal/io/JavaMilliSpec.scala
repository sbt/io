package sbt.io

import org.scalatest.FlatSpec

final class JavaMilliSpec extends FlatSpec {
  "JavaMilli" should "be exposed to the sbt.io package" in
    assertCompiles("""sbt.io.JavaMilli.getModifiedTime("/tmp")""")
}
