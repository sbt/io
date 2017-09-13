package sbt.io

import java.io.{ File => JFile }
import org.scalatest._
import sbt.io.syntax._
import java.nio.file.attribute.PosixFilePermission

class FileSpec extends FlatSpec with Matchers {
  "files" should "set/unset permissions" in {
    IO.withTemporaryDirectory { dir =>
      val t1 = dir / "foo.txt"
      IO.write(t1, "foo")

      //an[UnsupportedOperationException] should be thrownBy t1.dosAttributes
      t1.permissions(PosixFilePermission.OWNER_EXECUTE) shouldBe false

      t1.addPermission(PosixFilePermission.OWNER_EXECUTE)
      t1.addPermission(PosixFilePermission.GROUP_WRITE)
      t1.testPermission(PosixFilePermission.OWNER_EXECUTE) shouldBe true
      t1.permissionsAsString shouldBe "rwxrw-r--"

      t1.removePermission(PosixFilePermission.OWNER_EXECUTE)
      t1.isOwnerExecutable shouldBe false
      t1.permissionsAsString shouldBe "rw-rw-r--"
    }
  }
}
