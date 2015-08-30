package eu.timepit.refined

import eu.timepit.refined.Result.{Failed, Passed}

sealed abstract class Result[T, P] extends Product with Serializable {

  def value: T

  def predicate: P

  def fold[A](ifPassed: (T, P) => A, ifFailed: (T, P) => A): A =
    this match {
      case Passed(t, p) => ifPassed(t, p)
      case Failed(t, p) => ifFailed(t, p)
    }

  def mapBoth[U, Q](f: T => U, g: P => Q): Result[U, Q] =
    fold((t, p) => Passed(f(t), g(p)), (t, p) => Failed(f(t), g(p)))

  def mapFst[U](f: T => U): Result[U, P] =
    mapBoth(f, identity)

  def isPassed: Boolean =
    fold((_, _) => true, (_, _) => false)

  def isFailed: Boolean =
    fold((_, _) => false, (_, _) => true)

  def show(implicit s: Show[T, P]): String =
    s.show(value)
}

object Result {

  final case class Passed[T, P](value: T, predicate: P) extends Result[T, P]
  final case class Failed[T, P](value: T, predicate: P) extends Result[T, P]

  def fromBoolean[T, P](b: Boolean, t: T, p: P): Result[T, P] =
    if (b) Passed(t, p) else Failed(t, p)


  implicit def showResult[T, P](implicit sa: Show[T, P]): Show[T, Result[T, P]] =
    new Show[T, Result[T, P]] {
      override def show(t: T): String = sa.show(t)
    }
}
