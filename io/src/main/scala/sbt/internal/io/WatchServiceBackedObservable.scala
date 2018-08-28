package sbt.internal.io

import java.nio.file.StandardWatchEventKinds.OVERFLOW
import java.nio.file.{ Files, Path, WatchKey }
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import sbt.io.FileTreeDataView.{ Entry, Observable }
import sbt.io.FileTreeView.AllPass
import sbt.io.{ FileTreeDataView, FileTreeView, Logger, TypedPath }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._

object WatchServiceBackedObservable {
  private val eventThreadId = new AtomicInteger(0)
}
import sbt.internal.io.WatchServiceBackedObservable._
private[sbt] class WatchServiceBackedObservable[+T](s: WatchState,
                                                    delay: FiniteDuration,
                                                    converter: TypedPath => T,
                                                    closeService: Boolean,
                                                    logger: Logger)
    extends Observable[T] {
  private[this] val closed = new AtomicBoolean(false)
  private[this] val observers = new Observers[T]
  private[this] val thread: Thread = {
    val entryConverter = Entry.converter(converter)
    var registered = s.registered
    val lock = new Object
    val latch = new CountDownLatch(1)
    new Thread(s"watch-state-event-thread-${eventThreadId.incrementAndGet()}") {
      setDaemon(true)
      start()
      if (!latch.await(5, TimeUnit.SECONDS))
        throw new IllegalStateException("Couldn't start event thread")
      @tailrec
      final def loopImpl(): Unit = {
        if (!closed.get) getFilesForKey(s.service.poll(delay)).foreach { entry =>
          if (entry.exists) observers.onCreate(entry) else observers.onDelete(entry)
        }
        if (!closed.get) loopImpl()
      }
      override def run(): Unit = {
        latch.countDown()
        try {
          loopImpl()
        } catch {
          case _: InterruptedException =>
        }
      }

      def getFilesForKey(key: WatchKey): Vector[Entry[T]] = key match {
        case null => Vector.empty
        case k =>
          val rawEvents = k.synchronized {
            val events = k.pollEvents.asScala.toVector
            k.reset()
            events
          }
          val keyPath = k.watchable.asInstanceOf[Path]
          val allEvents = rawEvents.flatMap {
            case e if e.kind.equals(OVERFLOW) =>
              handleOverflow(k)
            case e if !e.kind.equals(OVERFLOW) && e.context != null =>
              Some(TypedPath(keyPath.resolve(e.context.asInstanceOf[Path])))
            case _ => None
          }
          logger.debug(s"Received events:\n${allEvents.mkString("\n")}")
          val (exist, notExist) = allEvents.partition(_.exists)
          val (updatedDirectories, updatedFiles) = exist.partition(_.isDirectory)
          val newFiles = updatedDirectories.flatMap(filesForNewDirectory)
          lock.synchronized { registered --= notExist.map(_.getPath) }
          notExist.foreach(p => s.unregister(p.getPath))
          (updatedFiles ++ newFiles ++ notExist).map(tp => Entry(tp, entryConverter))
      }

      /*
       * In the case of an overflow, we must poll the file system to find out if there are added
       * or removed directories. When there are new directories, we also want to return file
       * events for the files that are found therein. Because an overflow is likely to occur while
       * a directory is still being modified, we poll repeatedly until we get the same list of
       * files consecutively. We will not trigger for any files that are updated while the WatchKey
       * is in the OVERFLOW state. There is no good way to fix this without caching mtimes for
       * all of the files, which I don't think is worth doing at this juncture.
       */
      private def handleOverflow(key: WatchKey): Vector[TypedPath] = lock.synchronized {
        val allFiles = new mutable.HashSet[TypedPath]
        def addNewFiles(): Unit = {
          allFiles.clear()
          val path = key.watchable.asInstanceOf[Path]
          val view = FileTreeView.DEFAULT
          view.list(path, maxDepth = Integer.MAX_VALUE, AllPass).foreach { typedPath =>
            allFiles += typedPath
            val path = typedPath.getPath
            if (typedPath.isDirectory && !registered.contains(path)) {
              registered += path -> s.register(path)
            }
          }
          ()
        }

        var oldFiles = mutable.Set.empty[TypedPath]
        do {
          oldFiles = allFiles
          addNewFiles()
        } while (oldFiles != allFiles)
        registered --= registered.collect {
          case (d, k) if !Files.exists(d) =>
            k.reset()
            k.cancel()
            d
        }
        allFiles.toVector
      }

      /*
       * Returns new files found in new directory and any subdirectories, assuming that there is
       * a recursive source with a base that is parent to the directory.
       */
      private def filesForNewDirectory(typedPath: TypedPath): Seq[TypedPath] =
        if (!closed.get()) {
          val dir = typedPath.getPath
          lazy val recursive =
            s.sources.exists(src => dir.startsWith(src.base.toPath) && src.recursive)
          if (!registered.contains(dir) && recursive) {
            val result = FileTreeView.DEFAULT.list(dir, maxDepth = Integer.MAX_VALUE, AllPass)
            val newDirs = (Seq(typedPath).view ++ result).collect {
              case tp if tp.isDirectory && !closed.get() => tp.getPath -> s.register(tp.getPath)
            }
            lock.synchronized { registered ++= newDirs }
            result.toVector
          } else Nil
        } else Nil
    }
  }
  override def addObserver(observer: FileTreeDataView.Observer[T]): Int =
    observers.addObserver(observer)

  override def removeObserver(handle: Int): Unit = {
    observers.removeObserver(handle)
    ()
  }
  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      thread.interrupt()
      thread.join(5.seconds.toMillis)
      if (closeService) s.close()
      logger.debug("Closed WatchServiceBackedObservable")
    }
  }
}
