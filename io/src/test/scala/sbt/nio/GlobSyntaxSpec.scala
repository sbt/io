package sbt.nio

import java.nio.file._

import org.scalatest.FlatSpec
import sbt.io.syntax._
import sbt.io.AllPassFilter
import sbt.nio.syntax._

import scala.util.{ Properties, Try }

class GlobSyntaxSpec extends FlatSpec {
  import GlobSyntaxSpec._
  "path builders" should "use `*` and `**` objects" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    assert(path / "*" == Glob(path, (1, 1), AllPass))
    assert(path / "**" == Glob(path, (1, Int.MaxValue), AllPass))
    assert(path / "*" / "**" == Glob(path, (2, Int.MaxValue), AllPass))
    assert(path / "*/**" == Glob(path, (2, Int.MaxValue), AllPass))
    // mix and match
    assert(Try(path / "*" / "**" / "*").isFailure)
  }
  they should "apply extension filters" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    assert(path / "*.txt" == Glob(path, (1, 1), "*.txt"))
    assert(path / "*.txt" == Glob(path, (1, 1), "*.txt"))
    assert(path.getParent / "bar/*.txt" == Glob(path, (1, 1), "*.txt"))
    assert(path / "**" / "*.txt" == Glob(path, (1, Int.MaxValue), "*.txt"))
    assert(path / "**/*.txt" == Glob(path, (1, Int.MaxValue), "*.txt"))
    assert(path.getParent / "bar/**/*.txt" == Glob(path, (1, Int.MaxValue), "*.txt"))
    assert(path / "*" / "*.txt" == Glob(path, (2, 2), "*.txt"))
    assert(path / "*/*.txt" == Glob(path, (2, 2), "*.txt"))
    assert(path.getParent / "bar/*/*.txt" == Glob(path, (2, 2), "*.txt"))
    assert(path / "*" / "**" / "*.txt" == Glob(path, (2, Int.MaxValue), "*.txt"))
    assert(path / "*/**/*.txt" == Glob(path, (2, Int.MaxValue), "*.txt"))
    assert(path.getParent / "bar/*/**/*.txt" == Glob(path, (2, Int.MaxValue), "*.txt"))
  }
  they should "apply prefix filters" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    assert(path / "foo*" == Glob(path, (1, 1), "foo*"))
    assert(path.getParent / "bar / foo*" == Glob(path, (1, 1), "foo*"))
    assert(path / "**" / "foo*" == Glob(path, (1, Int.MaxValue), "foo*"))
    assert(path / "**/foo*" == Glob(path, (1, Int.MaxValue), "foo*"))
    assert(path.getParent / "bar/**/foo*" == Glob(path, (1, Int.MaxValue), "foo*"))
    assert(path / "*" / "foo*" == Glob(path, (2, 2), "foo*"))
    assert(path / "*/foo*" == Glob(path, (2, 2), "foo*"))
    assert(path.getParent / "bar/*/foo*" == Glob(path, (2, 2), "foo*"))
    assert(path / "*" / "**" / "foo*" == Glob(path, (2, Int.MaxValue), "foo*"))
    assert(path / "*/**/foo*" == Glob(path, (2, Int.MaxValue), "foo*"))
    assert(path.getParent / "bar/*/**/foo*" == Glob(path, (2, Int.MaxValue), "foo*"))
  }
  they should "apply suffix filters" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    assert(path / "*bar" == Glob(path, (1, 1), "*bar"))
    assert(path.getParent / "bar/*bar" == Glob(path, (1, 1), "*bar"))
    assert(path / "**" / "*bar" == Glob(path, (1, Int.MaxValue), "*bar"))
    assert(path / "**/*bar" == Glob(path, (1, Int.MaxValue), "*bar"))
    assert(path / "*" / "*bar" == Glob(path, (2, 2), "*bar"))
    assert(path / "*/*bar" == Glob(path, (2, 2), "*bar"))
    assert(path.getParent / "bar/*/*bar" == Glob(path, (2, 2), "*bar"))
    assert(path / "*" / "**" / "*bar" == Glob(path, (2, Int.MaxValue), "*bar"))
    assert(path / "*/**/*bar" == Glob(path, (2, Int.MaxValue), "*bar"))
    assert(path.getParent / "bar / */**/*bar" == Glob(path, (2, Int.MaxValue), "*bar"))
  }
  they should "apply split filters" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    assert(path / "foo*bar" == Glob(path, (1, 1), "foo*bar"))
    assert(path.getParent / "bar/foo*bar" == Glob(path, (1, 1), "foo*bar"))
    assert(path / "**" / "foo*bar" == Glob(path, (1, Int.MaxValue), "foo*bar"))
    assert(path / "**/foo*bar" == Glob(path, (1, Int.MaxValue), "foo*bar"))
    assert(path.getParent / "bar/**/foo*bar" == Glob(path, (1, Int.MaxValue), "foo*bar"))
    assert(path / "*" / "foo*bar" == Glob(path, (2, 2), "foo*bar"))
    assert(path / "*/foo*bar" == Glob(path, (2, 2), "foo*bar"))
    assert(path.getParent / "bar/*/foo*bar" == Glob(path, (2, 2), "foo*bar"))
    assert(path / "*" / "**" / "foo*bar" == Glob(path, (2, Int.MaxValue), "foo*bar"))
    assert(path / "*/**/foo*bar" == Glob(path, (2, Int.MaxValue), "foo*bar"))
    assert(path.getParent / "bar/*/**/foo*bar" == Glob(path, (2, Int.MaxValue), "foo*bar"))
  }
  they should "work with file syntax" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    val file = path.toFile
    assert(file * AllPassFilter == Glob(path, (1, 1), AllPass))
    assert(file ** AllPassFilter == Glob(path, (1, Int.MaxValue), AllPass))
    assert(file * "*.txt" == Glob(path, (1, 1), "*.txt"))
    assert(file ** "*.txt" == Glob(path, (1, Int.MaxValue), "*.txt"))
  }
  they should "convert strings" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    assert((s"$path/*": Glob) == Glob(path, (1, 1), AllPass))
    assert((s"$path/**": Glob) == Glob(path, (1, Int.MaxValue), AllPass))
    assert((s"$path/*/*.txt": Glob) == Glob(path, (2, 2), "*.txt"))
    assert((s"$path/**/*.txt": Glob) == Glob(path, (1, Int.MaxValue), "*.txt"))
    assert((s"$path/*/*/*.txt": Glob) == Glob(path, (3, 3), "*.txt"))
    assert((s"$path/*/**/*.txt": Glob) == Glob(path, (2, Int.MaxValue), "*.txt"))
  }
  "show" should "represent globs like the shell" in {
    val path = Paths.get("/foo/bar").toAbsolutePath
    assert((path / "foo.txt").toString == p"$path/foo.txt")
    assert((path / "*").toString == p"$path/*")
    assert((s"$path/*": Glob).toString == p"$path/*")
    // extensions
    assert((path / "*.txt").toString == p"$path/*.txt")

    assert((path / new ExtensionFilter("txt", "md")).toString == p"$path/(*.txt | *.md)")
    assert((path.getParent / "bar" / "baz": Glob).toString == p"$path/baz")
  }
}
object GlobSyntaxSpec {
  implicit class StringPathOps(val sc: StringContext) extends AnyVal {
    def p(args: Any*): String = {
      val raw = (sc.parts
        .drop(1)
        .zip(args))
        .map { case (a, p) => s"$p$a" }
        .mkString("")
      if (Properties.isWin) raw.replaceAll("/", "\\\\") else raw
    }
  }
}
