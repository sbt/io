package sbt.io

import java.io.{ File => JFile }
import org.scalatest._

class IOSyntaxSpec extends FlatSpec with Matchers {
  "file(...)" should "create File" in {
    import sbt.io.syntax._
    file(".") shouldBe (new JFile("."))
  }
  "file(...) / \"a\"" should "create File" in {
    import sbt.io.syntax._
    (file("project") / "build.properties") shouldBe (new JFile(new JFile("project"), "build.properties"))
  }
  "file(...) glob \"*.properties\"" should "create PathFinder" in {
    import sbt.io.syntax._
    (file("project") glob "*.properties").get shouldBe Seq(new JFile(new JFile("project"), "build.properties"))
  }
}
