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

  def replace[A](ifPassed: A, ifFailed: A): A =
    fold((_, _) => ifPassed, (_, _) => ifFailed)

  def mapBoth[U, Q](f: T => U, g: P => Q): Result[U, Q] =
    fold((t, p) => Passed(f(t), g(p)), (t, p) => Failed(f(t), g(p)))

  def mapFst[U](f: T => U): Result[U, P] =
    mapBoth(f, identity)

  def isPassed: Boolean =
    replace(true, false)

  def isFailed: Boolean =
    replace(false, true)

  def toVerb: String =
    replace("passed", "failed")

  def describe: String =
    s"Predicate $toVerb"

  def describeWith(detail: String): String =
    s"Predicate $detail $toVerb"
}

object Result {

  final case class Passed[T, P](value: T, predicate: P) extends Result[T, P]
  final case class Failed[T, P](value: T, predicate: P) extends Result[T, P]

  def fromBoolean[T, P](b: Boolean, t: T, p: P): Result[T, P] =
    if (b) Passed(t, p) else Failed(t, p)
}
