package sbt.nio

import java.nio.file._

trait PathFilter {
  def accept(path: Path): Boolean

  /** Constructs a filter that accepts a `Path` if it matches either this filter or the given `filter`. */
  def ||(filter: PathFilter): PathFilter = new OrFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it matches both this filter and the given `filter`. */
  def &&(filter: PathFilter): PathFilter = new AndFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it does not match this filter. */
  def unary_- : PathFilter = new NotFilter(this)
}
private[nio] object PathFilter {
  private[nio] final class FromFileFilter(private val filter: String => Boolean)
      extends PathNameFilter {
    override def accept(name: String): Boolean = filter(name)
    override def accept(path: Path): Boolean = accept(path.toString)
    override def equals(o: Any): Boolean = o match {
      case that: FromFileFilter => this.filter == that.filter
      case _                    => false
    }
    override def hashCode: Int = filter.hashCode
    override def toString: String = s"FullNameFilter($filter)"
  }
}
trait PathNameFilter extends PathFilter {
  def accept(name: String): Boolean
  override def accept(path: Path): Boolean = accept(path.getFileName.toString)

  /** Constructs a filter that accepts a `Path` if it matches either this filter or the given `filter`. */
  def |(filter: PathNameFilter): PathNameFilter = new OrNameFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it matches both this filter and the given `filter`. */
  def &(filter: PathNameFilter): PathNameFilter = new AndNameFilter(this, filter)
}
private[nio] class RelativePathFilter(private val base: Path, private val rest: String)
    extends PathFilter {
  private[this] val filter = FileSystems.getDefault.getPathMatcher(s"glob:$rest")
  override def accept(path: Path): Boolean = {
    if (path.startsWith(base)) {
      val relative = base.relativize(path)
      filter.matches(relative)
    } else false
  }
  override def equals(obj: Any): Boolean = obj match {
    case that: RelativePathFilter => this.base == that.base && this.rest == that.rest
    case _                        => false
  }
  override def hashCode: Int = (base.hashCode * 31) ^ rest.hashCode
  override def toString: String = rest
}
private[nio] object PathNameFilter {
  implicit def stringToPathNameFilter(string: String): PathNameFilter = apply(string)
  private[nio] def apply(nameFilter: String): PathNameFilter = {
    nameFilter.indexWhere(Glob.meta.contains(_)) match {
      case -1 =>
        nameFilter.indexOf('*') match {
          case -1 => new ExactNameFilter(nameFilter)
          case i if i == 0 && i < nameFilter.length - 2 && nameFilter(i + 1) == '.' =>
            new ExtensionFilter(nameFilter.substring(i + 2))
          case i =>
            val suffix = if (i < nameFilter.length - 2) nameFilter.substring(i + 1) else ""
            if (!suffix.contains('*')) new SplitFilter(nameFilter.substring(0, i), suffix)
            else new GlobPathNameFilter(nameFilter)
        }
      case _ => new GlobPathNameFilter(nameFilter)
    }
  }
}

private[nio] class GlobPathNameFilter(val nameFilter: String) extends PathNameFilter {
  private[this] val pathNameFilter = FileSystems.getDefault.getPathMatcher(s"glob:$nameFilter")
  override def accept(name: String): Boolean = pathNameFilter.matches(Paths.get(name))
  override def equals(o: Any): Boolean = o match {
    case that: GlobPathNameFilter => this.nameFilter == that.nameFilter
    case _                        => false
  }
  override def hashCode: Int = nameFilter.hashCode
  override def toString: String = nameFilter
}
private[nio] abstract class AbstractAndFilter[T <: PathFilter](val left: T,
                                                               val right: T,
                                                               private[this] val sep: String) {
  override def equals(o: Any): Boolean = o match {
    case that: AbstractAndFilter[_] => this.left == that.left && this.right == that.right
    case _                          => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left $sep $right"
}
private[nio] final class AndFilter(private[this] val left: PathFilter,
                                   private[this] val right: PathFilter)
    extends AbstractAndFilter(left, right, "&&")
    with PathFilter {
  override def accept(path: Path): Boolean = left.accept(path) && right.accept(path)
}
private[nio] final class AndNameFilter(private[this] val left: PathNameFilter,
                                       private[this] val right: PathNameFilter)
    extends AbstractAndFilter(left, right, "&")
    with PathNameFilter {
  override def accept(name: String): Boolean = left.accept(name) && right.accept(name)
}
private[nio] abstract class AbstractNotFilter[T <: PathFilter](val filter: T) extends PathFilter {
  override def accept(path: Path): Boolean = !filter.accept(path)
  override def equals(o: Any): Boolean = o match {
    case that: AbstractNotFilter[_] => this.filter == that.filter
    case _                          => false
  }
  override def hashCode: Int = filter.hashCode
  override def toString = s"!$filter"
}
private[nio] final class NotFilter(val f: PathFilter) extends AbstractNotFilter(f)
private[nio] final class NotNameFilter(val f: PathNameFilter)
    extends AbstractNotFilter(f)
    with PathNameFilter {
  override def accept(name: String): Boolean = !f.accept(name)
}
private[nio] abstract class AbstractOrFilter[T <: PathFilter](val left: T,
                                                              val right: T,
                                                              private[this] val sep: String) {
  override def equals(o: Any): Boolean = o match {
    case that: AbstractOrFilter[T] => this.left == that.left && this.right == that.right
    case _                         => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left $sep $right"
}
private[nio] final class OrFilter(private[this] val left: PathFilter,
                                  private[this] val right: PathFilter)
    extends AbstractOrFilter(left, right, "||")
    with PathFilter {
  override def accept(path: Path): Boolean = left.accept(path) || right.accept(path)
}
private[nio] final class OrNameFilter(private[this] val left: PathNameFilter,
                                      private[this] val right: PathNameFilter)
    extends AbstractOrFilter(left, right, "&")
    with PathNameFilter {
  override def accept(name: String): Boolean = left.accept(name) || right.accept(name)
}
final class ExtensionFilter(val extensions: String*) extends PathNameFilter {
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

  /** Constructs a filter that accepts a `Path` if it matches either this filter or the given `filter`. */
  override def |(filter: PathNameFilter): PathNameFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions ++ that.extensions: _*)
    case _                     => super.|(filter)
  }
  override def toString: String = {
    val parts = extensions.view.map("*." + _)
    if (extensions.size == 1) parts.mkString else parts.mkString("(", " | ", ")")
  }
}

private[nio] final class ExactPathFilter(private val path: Path) extends PathFilter {
  override def accept(other: Path): Boolean = other == path
  override def toString: String = path.toString
  override def equals(o: Any): Boolean = o match {
    case that: ExactPathFilter => this.path == path
    case _                     => false
  }
  override def hashCode: Int = path.hashCode
}
private[nio] final class ExactNameFilter(private val fileName: String) extends PathNameFilter {
  override def accept(name: String): Boolean = name == fileName
  override def toString: String = fileName
  override def equals(o: Any): Boolean = o match {
    case that: ExactNameFilter => this.fileName == that.fileName
    case _                     => false
  }
  override def hashCode: Int = fileName.hashCode
}

private[nio] final class SplitFilter(private val prefix: String, private val suffix: String)
    extends PathNameFilter {
  override def accept(name: String): Boolean = name.startsWith(prefix) && name.endsWith(suffix)
  override def toString: String = s"$prefix*$suffix"
  override def equals(o: Any): Boolean = o match {
    case that: SplitFilter => this.prefix == that.prefix && this.suffix == that.suffix
    case _                 => false
  }
  override def hashCode: Int = (prefix.hashCode * 31) ^ suffix.hashCode
}

case object AllPass extends PathNameFilter {
  override def accept(name: String): Boolean = true
  override def accept(path: Path): Boolean = true
}
case object NoPass extends PathNameFilter {
  override def accept(name: String): Boolean = false
  override def accept(path: Path): Boolean = false
}
