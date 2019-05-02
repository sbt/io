package sbt.nio.file

import java.nio.file.Path

/**
 * Represents a set of possible file changes.
 *
 * @param created the files that are newly created
 * @param deleted the files that have been deleted
 * @param updated the files that have been updated
 */
final case class ChangedFiles(created: Seq[Path], deleted: Seq[Path], updated: Seq[Path])
