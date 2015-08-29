package eu.timepit.refined

sealed trait Result[T, P] {

  def value: T

  def predicate: P

  def fold[A](ifPassed: Result[T, P] => A, ifFailed: Result[T, P] => A): A =
    this match {
      case Passed(_, _) => ifPassed(this)
      case Failed(_, _) => ifFailed(this)
    }

  def isPassed: Boolean =
    fold(_ => true, _ => false)
}

case class Passed[T, P](value: T, predicate: P) extends Result[T, P]
case class Failed[T, P](value: T, predicate: P) extends Result[T, P]
