package sbt.internal.io

import sbt.io.Alternative

private[sbt] object Alternatives {
  import sbt.io.syntax._
  final def alternatives[A, B](alts: Vector[A => Option[B]]): A => Option[B] =
    alts match {
      case Vector(f, fs @ _*) => alternative(f) | alternatives(fs.toVector)
      case Vector()           => a => None
    }

  implicit def alternative[A, B](f: A => Option[B]): Alternative[A, B] =
    new Alternative[A, B] {
      def |(g: A => Option[B]) =
        (a: A) => f(a) orElse g(a)
    }
}
