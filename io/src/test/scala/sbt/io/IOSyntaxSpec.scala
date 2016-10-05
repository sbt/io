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
    IO.withTemporaryDirectory { dir =>
      IO.write(new JFile(dir, "foo.txt"), "foo")
      IO.write(new JFile(dir, "bar.json"), "{}")
      (dir glob "*.txt").get shouldBe Seq(new JFile(dir, "foo.txt"))
    }
  }
}
