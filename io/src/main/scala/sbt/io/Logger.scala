package sbt.io

private[sbt] trait Logger {
  def debug(msg: => Any): Unit
}
private[sbt] object NullLogger extends Logger {
  private def ignoreArg[T](f: => T): Unit = if (false) { f; () } else ()
  override def debug(msg: => Any): Unit = ignoreArg(msg)
}
