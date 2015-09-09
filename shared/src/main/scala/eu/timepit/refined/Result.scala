package eu.timepit.refined

import eu.timepit.refined.Result.{Failed, Passed}

sealed abstract class Result[A] extends Product with Serializable {

  def detail: A

  def fold[B](ifPassed: A => B, ifFailed: A => B): B =
    this match {
      case Passed(d) => ifPassed(d)
      case Failed(d) => ifFailed(d)
    }

  def replace[B](ifPassed: B, ifFailed: B): B =
    fold(_ => ifPassed, _ => ifFailed)

  def map[B](f: A => B): Result[B] =
    fold(d => Passed(f(d)), d => Failed(f(d)))

  def isPassed: Boolean =
    replace(true, false)

  def isFailed: Boolean =
    replace(false, true)
}

object Result {

  final case class Passed[A](detail: A) extends Result[A]
  final case class Failed[A](detail: A) extends Result[A]

  def fromBoolean[A](b: Boolean, detail: A): Result[A] =
    if (b) Passed(detail) else Failed(detail)
}
