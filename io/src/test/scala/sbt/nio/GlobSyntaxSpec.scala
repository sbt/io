package sbt.nio

import org.scalatest.FlatSpec
import sbt.io.AllPassFilter
import sbt.io.syntax._
import sbt.nio.TestHelpers._
import sbt.nio.syntax._

class GlobSyntaxSpec extends FlatSpec {
  "path builders" should "use `*` and `**` objects" in {
    assert((basePath / "*").filter.accept(basePath.resolve("foo")))
    assert(!(basePath / "*").filter.accept(basePath.resolve("foo").resolve("bar")))
    assert(!(basePath / "*").filter.accept(basePath.getParent))
    assert(basePath / "*" == Glob(basePath, (1, 1), AllPass))
    assert(basePath / "**" == Glob(basePath, (1, Int.MaxValue), AllPass))
    assert(basePath / "*" / "**" == Glob(basePath, (2, Int.MaxValue), AllPass))
    assert(basePath / p"*/**" == Glob(basePath, (2, Int.MaxValue), AllPass))
  }
  they should "apply question marks" in {
    assert((basePath / "?foo").filter.accept(basePath.resolve("afoo")))
    assert(!(basePath / "?foo").filter.accept(basePath.resolve("afoob")))
    assert((basePath / "?foo?").filter.accept(basePath.resolve("afoob")))
    assert(!(basePath / "?foo?").filter.accept(basePath.resolve("afoo")))
    assert(!(basePath / "?foo?").filter.accept(basePath.resolve("afoobc")))
    assert((basePath / "?foo*").filter.accept(basePath.resolve("afoobc")))
    assert((basePath / "foo?").filter.accept(basePath.resolve("fooa")))
    assert((basePath / "foo?a").filter.accept(basePath.resolve("fooaa")))
  }
  they should "set range" in {
    assert((basePath / "?foo" / "*").range == ((2, 2)))
  }
  they should "apply ranges" in {
    assert((basePath / "foo[a-d]b").filter.accept(basePath.resolve("fooab")))
    assert(!(basePath / "foo[a-d]b").filter.accept(basePath.resolve("fooeb")))
    assert((basePath / "*foo[a-d]b").filter.accept(basePath.resolve("fooab")))
    assert((basePath / "*foo[a-d]b").filter.accept(basePath.resolve("abcdefooab")))
    assert(!(basePath / "foo[a-d]b").filter.accept(basePath.resolve("abcdefooeb")))
    assert((basePath / p"**/*foo[a-d]b").filter.accept(basePath.resolve("abcdefooab")))
    assert(
      (basePath / p"**/*/*foo[a-d]b").filter.accept(basePath.resolve("bar").resolve("abcdefooab")))
    assert(
      (basePath / p"**/*/*foo[a-d]b").filter
        .accept(basePath.resolve("bar/baz/buzz").resolve("abcdefooab")))
  }
  they should "apply extension filters" in {
    assert(basePath / "*.txt" == Glob(basePath, (1, 1), "*.txt"))
    assert(basePath / "*.txt" == Glob(basePath, (1, 1), "*.txt"))
    assert(basePath.getParent / p"bar/*.txt" == Glob(basePath, (1, 1), "*.txt"))
    assert(basePath / "**" / "*.txt" == Glob(basePath, (1, Int.MaxValue), "*.txt"))
    assert(basePath / p"**/*.txt" == Glob(basePath, (1, Int.MaxValue), "*.txt"))
    assert(basePath.getParent / p"bar/**/*.txt" == Glob(basePath, (1, Int.MaxValue), "*.txt"))
    assert(basePath / "*" / "*.txt" == Glob(basePath, (2, 2), "*.txt"))
    assert(basePath / p"*/*.txt" == Glob(basePath, (2, 2), "*.txt"))
    assert(basePath.getParent / p"bar/*/*.txt" == Glob(basePath, (2, 2), "*.txt"))
    assert(basePath / "*" / "**" / "*.txt" == Glob(basePath, (2, Int.MaxValue), "*.txt"))
    assert(basePath / p"*/**/*.txt" == Glob(basePath, (2, Int.MaxValue), "*.txt"))
    assert(basePath.getParent / p"bar/*/**/*.txt" == Glob(basePath, (2, Int.MaxValue), "*.txt"))
  }
  they should "apply prefix filters" in {
    assert(basePath / "foo*" == Glob(basePath, (1, 1), "foo*"))
    assert(basePath.getParent / p"bar/foo*" == Glob(basePath, (1, 1), "foo*"))
    assert(basePath / "**" / "foo*" == Glob(basePath, (1, Int.MaxValue), "foo*"))
    assert(basePath / p"**/foo*" == Glob(basePath, (1, Int.MaxValue), "foo*"))
    assert(basePath.getParent / p"bar/**/foo*" == Glob(basePath, (1, Int.MaxValue), "foo*"))
    assert(basePath / "*" / "foo*" == Glob(basePath, (2, 2), "foo*"))
    assert(basePath / p"*/foo*" == Glob(basePath, (2, 2), "foo*"))
    assert(basePath.getParent / p"bar/*/foo*" == Glob(basePath, (2, 2), "foo*"))
    assert(basePath / "*" / "**" / "foo*" == Glob(basePath, (2, Int.MaxValue), "foo*"))
    assert(basePath / p"*/**/foo*" == Glob(basePath, (2, Int.MaxValue), "foo*"))
    assert(basePath.getParent / p"bar/*/**/foo*" == Glob(basePath, (2, Int.MaxValue), "foo*"))
  }
  they should "apply suffix filters" in {
    assert(basePath / "*bar" == Glob(basePath, (1, 1), "*bar"))
    assert(basePath.getParent / p"bar/*bar" == Glob(basePath, (1, 1), "*bar"))
    assert(basePath / "**" / "*bar" == Glob(basePath, (1, Int.MaxValue), "*bar"))
    assert(basePath / p"**/*bar" == Glob(basePath, (1, Int.MaxValue), "*bar"))
    assert(basePath / "*" / "*bar" == Glob(basePath, (2, 2), "*bar"))
    assert(basePath / p"*/*bar" == Glob(basePath, (2, 2), "*bar"))
    assert(basePath.getParent / p"bar/*/*bar" == Glob(basePath, (2, 2), "*bar"))
    assert(basePath / "*" / "**" / "*bar" == Glob(basePath, (2, Int.MaxValue), "*bar"))
    assert(basePath / p"*/**/*bar" == Glob(basePath, (2, Int.MaxValue), "*bar"))
    assert(basePath.getParent / p"bar/*/**/*bar" == Glob(basePath, (2, Int.MaxValue), "*bar"))
  }
  they should "apply split filters" in {
    assert(basePath / "foo*bar" == Glob(basePath, (1, 1), "foo*bar"))
    assert(basePath.getParent / p"bar/foo*bar" == Glob(basePath, (1, 1), "foo*bar"))
    assert(basePath / "**" / "foo*bar" == Glob(basePath, (1, Int.MaxValue), "foo*bar"))
    assert(basePath / p"**/foo*bar" == Glob(basePath, (1, Int.MaxValue), "foo*bar"))
    assert(basePath.getParent / p"bar/**/foo*bar" == Glob(basePath, (1, Int.MaxValue), "foo*bar"))
    assert(basePath / "*" / p"foo*bar" == Glob(basePath, (2, 2), "foo*bar"))
    assert(basePath / p"*/foo*bar" == Glob(basePath, (2, 2), "foo*bar"))
    assert(basePath.getParent / p"bar/*/foo*bar" == Glob(basePath, (2, 2), "foo*bar"))
    assert(basePath / "*" / "**" / "foo*bar" == Glob(basePath, (2, Int.MaxValue), "foo*bar"))
    assert(basePath / p"*/**/foo*bar" == Glob(basePath, (2, Int.MaxValue), "foo*bar"))
    assert(basePath.getParent / p"bar/*/**/foo*bar" == Glob(basePath, (2, Int.MaxValue), "foo*bar"))
  }
  they should "work with file syntax" in {
    val file = basePath.toFile
    assert(file * AllPassFilter == Glob(basePath, (1, 1), AllPass))
    assert(file ** AllPassFilter == Glob(basePath, (1, Int.MaxValue), AllPass))
    assert(file * "*.txt" == Glob(basePath, (1, 1), "*.txt"))
    assert(file ** "*.txt" == Glob(basePath, (1, Int.MaxValue), "*.txt"))
  }
  they should "convert strings" in {
    assert((p"$basePath/*": Glob) == Glob(basePath, (1, 1), AllPass))
    assert((p"$basePath/**": Glob) == Glob(basePath, (1, Int.MaxValue), AllPass))
    assert((p"$basePath/*/*.txt": Glob) == Glob(basePath, (2, 2), "*.txt"))
    assert((p"$basePath/**/*.txt": Glob) == Glob(basePath, (1, Int.MaxValue), "*.txt"))
    assert((p"$basePath/*/*/*.txt": Glob) == Glob(basePath, (3, 3), "*.txt"))
    assert((p"$basePath/*/**/*.txt": Glob) == Glob(basePath, (2, Int.MaxValue), "*.txt"))
  }
  "show" should "represent globs like the shell" in {
    assert((basePath / "foo.txt").toString == p"$basePath/foo.txt")
    assert((basePath / "*").toString == p"$basePath/*")
    assert((p"$basePath/*": Glob).toString == p"$basePath/*")
    // extensions
    assert((basePath / "*.txt").toString == p"$basePath/*.txt")

    assert((basePath / new ExtensionFilter("txt", "md")).toString == p"$basePath/(*.txt | *.md)")
    assert((basePath.getParent / "bar" / "baz": Glob).toString == p"$basePath/baz")
  }
}
