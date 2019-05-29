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

import java.io.{ File, IOException }
import java.nio.file.Files
import java.util.regex.Pattern

/** A `java.io.FileFilter` with additional methods for combining filters. */
trait FileFilter extends java.io.FileFilter {

  /** Constructs a filter that accepts a `File` if it matches either this filter or the given `filter`. */
  def ||(filter: FileFilter): FileFilter = new OrFilter(this, filter)

  /** Constructs a filter that accepts a `File` if it matches both this filter and the given `filter`. */
  def &&(filter: FileFilter): FileFilter = new AndFilter(this, filter)

  /** Constructs a filter that accepts a `File` if it matches this filter but does not match the given `filter`. */
  def --(filter: FileFilter): FileFilter = new AndFilter(this, new NotFilter(filter))

  /** Constructs a filter that accepts a `File` if it does not match this filter. */
  def unary_- : FileFilter = new NotFilter(this)
}

/** A filter on Strings.  This also functions as a [[FileFilter]] by applying the String filter to the value of a File's `getName`. */
trait NameFilter extends FileFilter {

  /** Returns `true` to include the `name`, `false` to exclude it. */
  def accept(name: String): Boolean

  /** Accepts `File` if its `getName` method is accepted by this filter. */
  final def accept(file: File): Boolean = accept(file.getName)

  /** Constructs a filter that accepts a `String` if it matches either this filter or the given `filter`. */
  def |(filter: NameFilter): NameFilter = new OrNameFilter(this, filter)

  /** Constructs a filter that accepts a `String` if it matches both this filter and the given `filter`. */
  def &(filter: NameFilter): NameFilter = new AndNameFilter(this, filter)

  /** Constructs a filter that accepts a `String` if it matches this filter but not the given `filter`. */
  def -(filter: NameFilter): NameFilter = new AndNameFilter(this, new NotNameFilter(filter))

  /** Constructs a filter that accepts a `String` if it does not match this filter. */
  override def unary_- : NameFilter = new NotNameFilter(this)
}

private[sbt] sealed trait AbstractAndFilter extends FileFilter {
  def left: FileFilter
  def right: FileFilter
  override def equals(o: Any): Boolean = o match {
    case that: AndFilter => this.left == that.left && this.right == that.right
    case _               => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left && $right"
}
final class AndFilter(override val left: FileFilter, override val right: FileFilter)
    extends AbstractAndFilter {
  override def accept(pathname: File): Boolean = left.accept(pathname) && right.accept(pathname)
}
final class AndNameFilter(override val left: NameFilter, override val right: NameFilter)
    extends AbstractAndFilter
    with NameFilter {
  override def accept(name: String): Boolean = left.accept(name) && right.accept(name)
}

private[sbt] sealed trait AbstractNotFilter extends FileFilter {
  def fileFilter: FileFilter
  override def equals(o: Any): Boolean = o match {
    case that: NotFilter => this.fileFilter == that.fileFilter
    case _               => false
  }
  override def hashCode: Int = -fileFilter.hashCode
  override def toString = s"!$fileFilter"
}
final class NotFilter(override val fileFilter: FileFilter) extends AbstractNotFilter {
  override def accept(pathname: File): Boolean = !fileFilter.accept(pathname)
}
final class NotNameFilter(override val fileFilter: NameFilter)
    extends AbstractNotFilter
    with NameFilter {
  override def accept(name: String): Boolean = !fileFilter.accept(name)
}

private[sbt] sealed trait AbstractOrFilter extends FileFilter {
  def left: FileFilter
  def right: FileFilter
  override def equals(o: Any): Boolean = o match {
    case that: AndFilter => this.left == that.left && this.right == that.right
    case _               => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left || $right"
}
final class OrFilter(override val left: FileFilter, override val right: FileFilter)
    extends AbstractOrFilter {
  override def accept(pathname: File): Boolean = left.accept(pathname) || right.accept(pathname)
}
final class OrNameFilter(override val left: NameFilter, override val right: NameFilter)
    extends AbstractOrFilter
    with NameFilter {
  override def accept(name: String): Boolean = left.accept(name) || right.accept(name)
}

/**
 * Represents a filter for files that end in a given list of extensions.
 *
 * @param extensions the extensions to accept
 */
final class ExtensionFilter(val extensions: String*) extends NameFilter {
  private val set = extensions.toSet

  /** Returns `true` to include the `name`, `false` to exclude it. */
  override def accept(name: String): Boolean = {
    val extension = name.lastIndexOf('.') match {
      case l if l >= 0 && l < name.length => name.substring(l + 1)
      case _                              => ""
    }
    set.contains(extension)
  }

  override def equals(o: Any): Boolean = o match {
    case that: ExtensionFilter => this.set == that.set
    case _                     => false
  }
  override lazy val hashCode: Int = extensions.hashCode

  /** Constructs a filter that accepts a `File` if it matches either this filter or the given `filter`. */
  override def |(filter: NameFilter): NameFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions ++ that.extensions: _*)
    case _                     => super.|(filter)
  }

  /** Constructs a filter that accepts a `File` if it matches both this filter and the given `filter`. */
  override def &(filter: NameFilter): NameFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions intersect that.extensions: _*)
    case _                     => super.&(filter)
  }

  /** Constructs a filter that accepts a `File` if it matches this filter but does not match the given `filter`. */
  override def -(filter: NameFilter): NameFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions diff that.extensions: _*)
    case _                     => super.-(filter)
  }

  /** Constructs a filter that accepts a `File` if it matches either this filter or the given `filter`. */
  override def ||(filter: FileFilter): FileFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions ++ that.extensions: _*)
    case _                     => super.||(filter)
  }

  /** Constructs a filter that accepts a `File` if it matches both this filter and the given `filter`. */
  override def &&(filter: FileFilter): FileFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions intersect that.extensions: _*)
    case _                     => super.&&(filter)
  }

  /** Constructs a filter that accepts a `File` if it matches this filter but does not match the given `filter`. */
  override def --(filter: FileFilter): FileFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions diff that.extensions: _*)
    case _                     => super.--(filter)
  }
  override def toString: String = s"ExtensionFilter(${extensions mkString ","})"
}

object ExtensionFilter {
  val ScalaOrJavaSource = new ExtensionFilter("scala", "java")
}

final class PrefixFilter(val prefix: String) extends NameFilter {
  override def accept(name: String): Boolean = name.startsWith(prefix)
  override def equals(o: Any): Boolean = o match {
    case that: PrefixFilter => this.prefix == that.prefix
    case _                  => false
  }
  override def hashCode: Int = prefix.hashCode
  override def toString: String = s"PrefixFilter($prefix)"
}

final class SuffixFilter(val suffix: String) extends NameFilter {
  override def accept(name: String): Boolean = name.endsWith(suffix)
  override def equals(o: Any): Boolean = o match {
    case that: SuffixFilter => this.suffix == that.suffix
    case _                  => false
  }
  override def hashCode: Int = suffix.hashCode
  override def toString: String = s"SuffixFilter($suffix)"
}

/** A [[FileFilter]] that selects files that are hidden according to `java.nio.file.Files.isHidden` or if they start with a dot (`.`). */
case object HiddenFileFilter extends FileFilter {
  def accept(file: File): Boolean =
    try Files.isHidden(file.toPath) && file.getName != "."
    catch { case _: IOException => false }
  override def unary_- : FileFilter = NotHiddenFileFilter
}
private[sbt] case object NotHiddenFileFilter extends FileFilter {
  def accept(file: File): Boolean = !HiddenFileFilter.accept(file)
  override def unary_- : FileFilter = HiddenFileFilter
}

/** A [[FileFilter]] that selects files that exist according to `java.io.File.exists`. */
case object ExistsFileFilter extends FileFilter {
  def accept(file: File): Boolean = file.exists
}

/** A [[FileFilter]] that selects files that are a directory according to `java.io.File.isDirectory`. */
case object DirectoryFilter extends FileFilter {
  def accept(file: File): Boolean = file.isDirectory
}

/** A [[FileFilter]] that selects files according the predicate `acceptFunction`. */
sealed class SimpleFileFilter(val acceptFunction: File => Boolean) extends FileFilter {
  final def accept(file: File): Boolean = acceptFunction(file)
  override def equals(o: Any): Boolean = o match {
    // Note that anonymous functions often get compiled to a constant value so this equality
    // check may be true more often than one might naively assume given that this is often
    // a reference comparison.
    case that: SimpleFileFilter => this.acceptFunction == that.acceptFunction
    case _                      => false
  }
  override def hashCode(): Int = acceptFunction.hashCode
  override def toString: String = s"SimpleFilter($acceptFunction)"
}

/** A [[FileFilter]] that only accepts a single input file. */
final class ExactFileFilter(val file: File) extends FileFilter {
  override def accept(f: File): Boolean = f == file
  override def toString: String = s"ExactFileFilter($file)"
  override def equals(o: Any): Boolean = o match {
    case that: ExactFileFilter => this.file == that.file
    case _                     => false
  }
  override def hashCode: Int = file.hashCode
}

/** A [[NameFilter]] that accepts a name if it is exactly equal to `matchName`. */
final class ExactFilter(val matchName: String) extends NameFilter {
  def accept(name: String): Boolean = matchName == name
  override def toString = s"ExactFilter($matchName)"
  override def equals(o: Any): Boolean = o match {
    case that: ExactFilter => this.matchName == that.matchName
    case _                 => false
  }
  override def hashCode(): Int = matchName.hashCode
}

/** A [[NameFilter]] that accepts a name if the predicate `acceptFunction` accepts it. */
sealed class SimpleFilter(val acceptFunction: String => Boolean) extends NameFilter {
  final def accept(name: String): Boolean = acceptFunction(name)

  override def equals(o: Any): Boolean = o match {
    // Note that anonymous functions often get compiled to a constant value so this equality
    // check may be true more often than one might naively assume given that this is often
    // a reference comparison.
    case that: SimpleFilter => this.acceptFunction == that.acceptFunction
    case _                  => false
  }
  override def hashCode(): Int = acceptFunction.hashCode
  override def toString: String = s"SimpleFilter($acceptFunction)"
}

/** A [[NameFilter]] that accepts a name if it matches the regular expression defined by `pattern`. */
final class PatternFilter(val parts: Seq[String], val pattern: Pattern) extends NameFilter {
  def this(pattern: Pattern) = this(Nil: Seq[String], pattern)
  private[this] val lock = new Object
  def accept(name: String): Boolean = lock.synchronized(pattern.matcher(name).matches)
  override def toString = s"PatternFilter($pattern)"
  override def equals(o: Any): Boolean = o match {
    case that: PatternFilter => this.pattern == that.pattern
    case _                   => false
  }
  override def hashCode(): Int = pattern.hashCode
}

/** A [[NameFilter]] that accepts all names. That is, `accept` always returns `true`. */
case object AllPassFilter extends NameFilter {
  def accept(name: String) = true
  override def unary_- : NameFilter = NothingFilter
}

/** A [[NameFilter]] that accepts nothing.  That is, `accept` always returns `false`. */
case object NothingFilter extends NameFilter {
  def accept(name: String) = false
  override def unary_- : NameFilter = AllPassFilter
}

object NameFilter {
  private class FunctionFilter(val f: String => Boolean) extends NameFilter {
    def accept(name: String): Boolean = f(name)
    override def toString = s"FunctionFilter($f)"
    override def equals(o: Any): Boolean = o match {
      case that: FunctionFilter => this.f == that.f
      case _                    => false
    }
    override def hashCode(): Int = f.hashCode
  }
  implicit def fnToNameFilter(f: String => Boolean): NameFilter = new FunctionFilter(f)
}
object FileFilter {

  /** Allows a String to be used where a `NameFilter` is expected and any asterisks (`*`) will be interpreted as wildcards.  See [[sbt.io.GlobFilter]].*/
  implicit def globFilter(s: String): NameFilter = GlobFilter(s)

}

/** Constructs a filter from a String, interpreting wildcards.  See the [[GlobFilter.apply]] method. */
object GlobFilter {

  /**
   * Constructs a [[NameFilter]] from a String, interpreting `*` as a wildcard.
   * Control characters, as determined by `java.lang.Character.isISOControl` are not allowed
   * due to the implementation restriction of using Java's `Pattern` and `Pattern.quote`,
   * which do not handle these characters.
   */
  def apply(expression: String): NameFilter = {
    require(
      !expression.exists(java.lang.Character.isISOControl),
      "Control characters not allowed in filter expression."
    )
    if (expression == "*")
      AllPassFilter
    else if (expression.indexOf('*') < 0) // includes case where expression is empty
      new ExactFilter(expression)
    else {
      val parts = expression.split("\\*", -1)
      parts match {
        case Array("", ext) if ext.startsWith(".") && !ext.drop(1).contains(".") =>
          new ExtensionFilter(ext.drop(1))
        case Array(prefix, "") => new PrefixFilter(prefix)
        case Array("", suffix) => new SuffixFilter(suffix)
        case _                 => new PatternFilter(parts, Pattern.compile(parts.map(quote).mkString(".*")))
      }
    }
  }

  private def quote(s: String) = if (s.isEmpty) "" else Pattern.quote(s.replaceAll("\n", """\n"""))

}
