/* sbt
 * Copyright 2009-2015 Typesafe, Inc, Mark Harrah, and others
 */
package sbt.internal.io

import annotation.tailrec
import sbt.io.PathFinder

private[sbt] object SourceModificationWatch {
  @tailrec def watch(sourcesFinder: PathFinder, pollDelayMillis: Int, state: WatchState)(terminationCondition: => Boolean): (Boolean, WatchState) =
    {
      import state._

      val sourceFiles: Iterable[java.io.File] = sourcesFinder.get
      val sourceFilesPath: Set[String] = sourceFiles.map(_.getCanonicalPath)(collection.breakOut)
      val lastModifiedTime =
        (0L /: sourceFiles) { (acc, file) => math.max(acc, file.lastModified) }

      val sourcesModified =
        lastModifiedTime > lastCallbackCallTime ||
          previousFiles != sourceFilesPath

      val (triggered, newCallbackCallTime) =
        if (sourcesModified)
          (false, System.currentTimeMillis)
        else
          (awaitingQuietPeriod, lastCallbackCallTime)

      val newState = new WatchState(newCallbackCallTime, sourceFilesPath, sourcesModified, if (triggered) count + 1 else count)
      if (triggered)
        (true, newState)
      else {
        Thread.sleep(pollDelayMillis)
        if (terminationCondition)
          (false, newState)
        else
          watch(sourcesFinder, pollDelayMillis, newState)(terminationCondition)
      }
    }
}
private[sbt] final class WatchState(val lastCallbackCallTime: Long, val previousFiles: Set[String], val awaitingQuietPeriod: Boolean, val count: Int) {
  def previousFileCount: Int = previousFiles.size
}

private[sbt] object WatchState {
  def empty = new WatchState(0L, Set.empty[String], false, 0)
}
