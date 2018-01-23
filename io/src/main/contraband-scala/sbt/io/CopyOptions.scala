/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.io
/** The options for the copy operation in `IO`. */
final class CopyOptions private (
  /**
   * A source file is always copied if `overwrite` is true.
   * If `overwrite` is false, the source is only copied if the target is missing or is older than the
   * source file according to last modified times.
   * If the source is a directory, the corresponding directory is created.
   */
  val overwrite: Boolean,
  /** If `true` the last modified times are copied. */
  val preserveLastModified: Boolean,
  /** If `true` the executable properties are copied. */
  val preserveExecutable: Boolean,
  /** If `true` the file permissions are copied. Setting this `true` disregards the effect of preserveExcecutable flag */
  val preservePermission: Boolean) extends Serializable {
  
  private def this() = this(false, false, true, true)
  private def this(overwrite: Boolean, preserveLastModified: Boolean, preserveExecutable: Boolean) = this(overwrite, preserveLastModified, preserveExecutable, true)
  
  override def equals(o: Any): Boolean = o match {
    case x: CopyOptions => (this.overwrite == x.overwrite) && (this.preserveLastModified == x.preserveLastModified) && (this.preserveExecutable == x.preserveExecutable) && (this.preservePermission == x.preservePermission)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (37 * (37 * (17 + "sbt.io.CopyOptions".##) + overwrite.##) + preserveLastModified.##) + preserveExecutable.##) + preservePermission.##)
  }
  override def toString: String = {
    "CopyOptions(" + overwrite + ", " + preserveLastModified + ", " + preserveExecutable + ", " + preservePermission + ")"
  }
  protected[this] def copy(overwrite: Boolean = overwrite, preserveLastModified: Boolean = preserveLastModified, preserveExecutable: Boolean = preserveExecutable, preservePermission: Boolean = preservePermission): CopyOptions = {
    new CopyOptions(overwrite, preserveLastModified, preserveExecutable, preservePermission)
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
  def withPreservePermission(preservePermission: Boolean): CopyOptions = {
    copy(preservePermission = preservePermission)
  }
}
object CopyOptions {
  
  def apply(): CopyOptions = new CopyOptions(false, false, true, true)
  def apply(overwrite: Boolean, preserveLastModified: Boolean, preserveExecutable: Boolean): CopyOptions = new CopyOptions(overwrite, preserveLastModified, preserveExecutable, true)
  def apply(overwrite: Boolean, preserveLastModified: Boolean, preserveExecutable: Boolean, preservePermission: Boolean): CopyOptions = new CopyOptions(overwrite, preserveLastModified, preserveExecutable, preservePermission)
}
