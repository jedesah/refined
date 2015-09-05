package eu.timepit.refined

import eu.timepit.refined.Result.{Failed, Passed}

import scala.util.{Failure, Success, Try}

trait Show[T, P, R] extends Serializable {

  final type Res = Result[T, R]

  def showExpr(t: T): String

  def showResult(r: Res): String =
    r match {
      case Passed(t, _) => s"Predicate passed: ${showExpr(t)}."
      case Failed(t, _) => s"Predicate failed: ${showExpr(t)}."
    }
}

object Show {

  type Flat[T, P] = Show[T, P, P]

  def instance[T, P, R](f: T => String): Show[T, P, R] =
    new Show[T, P, R] {
      override def showExpr(t: T): String = f(t)
    }

  def fromPartial[T, U, P](pf: T => U, name: String): Show.Flat[T, P] =
    new Show[T, P, P] {
      override def showExpr(t: T): String = s"""isValid$name("$t")"""

      override def showResult(r: Res): String =
        Try(pf(r.value)) match {
          case Success(_) => s"$name predicate passed."
          case Failure(e) => s"$name predicate failed: ${e.getMessage}"
        }
    }
}
