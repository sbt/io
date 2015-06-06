package sbt.internal.io

private[sbt] trait Alternative[A, B] { def |(g: A => Option[B]): A => Option[B] }
private[sbt] trait Alternatives {
  implicit def alternative[A, B](f: A => Option[B]): Alternative[A, B] =
    new Alternative[A, B] {
      def |(g: A => Option[B]) =
        (a: A) => f(a) orElse g(a)
    }
  final def alternatives[A, B](alts: Seq[A => Option[B]]): A => Option[B] =
    alts match {
      case Seq(f, fs @ _*) => f | alternatives(fs)
      case Seq()           => a => None
    }
}
private[sbt] object Alternatives extends Alternatives
