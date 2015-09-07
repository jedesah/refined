package eu.timepit.refined

import eu.timepit.refined.Result.{Failed, Passed}

import scala.util.{Failure, Success, Try}

trait Show[T, P, R] extends Serializable { self =>

  final type Res = Result[T, R]

  def showExpr(t: T): String

  def showResult(r: Res): String =
    s"${r.describe}: ${showExpr(r.value)}."

  def accumulateShowExpr(t: T): List[String] =
    List(showExpr(t))

  def contramap[U](f: U => T): Show[U, P, R] =
    new Show[U, P, R] {
      override def showExpr(t: U): String = self.showExpr(f(t))
      override def showResult(r: Res): String = self.showResult(r.mapFst(f))
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
      override def showExpr(t: T): String =
        s"""isValid$name("$t")"""

      override def showResult(r: Res): String = {
        val prefix = s"$name predicate ${r.toVerb}"
        val suffix = Try(pf(r.value)) match {
          case Success(_) => "."
          case Failure(e) => ": " + e.getMessage
        }
        prefix + suffix
      }
    }
}
