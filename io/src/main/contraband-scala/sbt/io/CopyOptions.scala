/**
 * This code is generated using [[https://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.io
/**
 * The options for the copy operation in `IO`.
 * @param overwrite A source file is always copied if `overwrite` is true.
                    If `overwrite` is false, the source is only copied if the target is missing or is older than the
                    source file according to last modified times.
                    If the source is a directory, the corresponding directory is created.
 * @param preserveLastModified If `true` the last modified times are copied.
 * @param preserveExecutable If `true` the executable properties are copied.
 */
final class CopyOptions private (
  val overwrite: Boolean,
  val preserveLastModified: Boolean,
  val preserveExecutable: Boolean) extends Serializable {
  
  private def this() = this(false, false, true)
  
  override def equals(o: Any): Boolean = this.eq(o.asInstanceOf[AnyRef]) || (o match {
    case x: CopyOptions => (this.overwrite == x.overwrite) && (this.preserveLastModified == x.preserveLastModified) && (this.preserveExecutable == x.preserveExecutable)
    case _ => false
  })
  override def hashCode: Int = {
    37 * (37 * (37 * (37 * (17 + "sbt.io.CopyOptions".##) + overwrite.##) + preserveLastModified.##) + preserveExecutable.##)
  }
  override def toString: String = {
    "CopyOptions(" + overwrite + ", " + preserveLastModified + ", " + preserveExecutable + ")"
  }
  private[this] def copy(overwrite: Boolean = overwrite, preserveLastModified: Boolean = preserveLastModified, preserveExecutable: Boolean = preserveExecutable): CopyOptions = {
    new CopyOptions(overwrite, preserveLastModified, preserveExecutable)
  }
  def withOverwrite(overwrite: Boolean): CopyOptions = {
    copy(overwrite = overwrite)
  }
  def withPreserveLastModified(preserveLastModified: Boolean): CopyOptions = {
    copy(preserveLastModified = preserveLastModified)
  }
  def withPreserveExecutable(preserveExecutable: Boolean): CopyOptions = {
    copy(preserveExecutable = preserveExecutable)
  }
}
object CopyOptions {
  
  def apply(): CopyOptions = new CopyOptions()
  def apply(overwrite: Boolean, preserveLastModified: Boolean, preserveExecutable: Boolean): CopyOptions = new CopyOptions(overwrite, preserveLastModified, preserveExecutable)
}
