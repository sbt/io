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

import java.io.File

object WithFiles {

  /**
   * Takes the relative path -> content pairs and writes the content to a file in a temporary directory.
   * The written file path is the relative path resolved against the temporary directory path.
   * The provided function is called with the resolved file paths in the same order as the inputs.
   */
  def apply[T](sources: (File, String)*)(f: Seq[File] => T): T =
    IO.withTemporaryDirectory { dir =>
      val sourceFiles =
        for ((file, content) <- sources) yield {
          assert(!file.isAbsolute)
          val to = new File(dir, file.getPath)
          IO.write(to, content)
          to
        }
      f(sourceFiles)
    }

}
