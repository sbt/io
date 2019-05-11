/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

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
