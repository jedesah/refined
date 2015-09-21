package eu.timepit.refined.api

import scala.util.{Failure, Success, Try}

trait Show[T, P] extends Serializable {

  type R

  final type Res = Result[R]

  def showExpr(t: T): String

  def showResult(t: T, r: Res): String =
    s"Predicate ${r.morph("passed", "failed")}: ${showExpr(t)}."
}

object Show {

  type Aux[T, P, R0] = Show[T, P] { type R = R0 }

  type Flat[T, P] = Aux[T, P, P]

  def apply[T, P](implicit s: Show[T, P]): Aux[T, P, s.R] = s

  def instance[T, P, R0](f: T => String): Show.Aux[T, P, R0] =
    new Show[T, P] {
      override type R = R0
      override def showExpr(t: T): String = f(t)
    }

  def fromPartial[T, U, P](pf: T => U, name: String): Show.Flat[T, P] =
    new Show[T, P] {
      override type R = P
      override def showExpr(t: T): String = s"""isValid$name("$t")"""

      override def showResult(t: T, r: Res): String =
        Try(pf(t)) match {
          case Success(_) => s"$name predicate passed."
          case Failure(e) => s"$name predicate failed: " + e.getMessage
        }

    }
}
