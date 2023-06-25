/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.nio

import java.nio.file.{ Files, WatchKey }
import java.nio.file.StandardWatchEventKinds._

import org.scalatest.flatspec.AnyFlatSpec
import sbt.internal.io.MacOSXWatchService
import sbt.io.{ IO, WatchService }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration.{ Deadline => SDeadline, _ }
import scala.util.Properties

class MacOSXWatchServiceSpec extends AnyFlatSpec {
  private def pollFor(service: WatchService, duration: FiniteDuration)(
      cond: WatchKey => Boolean
  ): Boolean = {
    val limit = duration.fromNow
    @tailrec
    def impl(): Boolean = {
      val remaining = limit - SDeadline.now
      if (remaining > 0.seconds) {
        service.poll(remaining) match {
          case k if cond(k) => true
          case _            => impl()
        }
      } else false
    }
    impl()
  }
  private def test(): Unit =
    if (Properties.isMac) IO.withTemporaryDirectory { dirFile =>
      val dir = dirFile.toPath.toRealPath()
      val foo = Files.createDirectories(dir.resolve("foo"))
      val foobar = Files.createDirectories(dir.resolve("foobar"))
      val service = new MacOSXWatchService
      try {
        service.register(foo, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)
        service.register(foobar, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)
        val fooFile = Files.createFile(foo.resolve("foo-file"))
        assert(pollFor(service, 5.seconds) { k =>
          k.pollEvents.asScala.exists(_.context == fooFile.getFileName())
        })
        val foobarFile = Files.createFile(foo.resolve("foo-bar-file"))
        assert(pollFor(service, 5.seconds) { k =>
          k.pollEvents.asScala.exists(_.context == foobarFile.getFileName())
        })
      } finally service.close()
      ()
    }
    else {}
  "MacOSXWatchService" should "handle overlapping directories" in test()
}
