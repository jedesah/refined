package eu.timepit.refined

import eu.timepit.refined.Result.{Failed, Passed}

import scala.util.{Failure, Success, Try}

trait Show[T, P, R] extends Serializable { self =>

  final type Res = Result[R]

  def showExpr(t: T): String

  def showResult(t: T, r: Res): String =
    s": ${showExpr(t)}."

  def accumulateShowExpr(t: T): List[String] =
    List(showExpr(t))

  def contramap[U](f: U => T): Show[U, P, R] =
    new Show[U, P, R] {
      override def showExpr(t: U): String = self.showExpr(f(t))
      override def showResult(t: U, r: Res): String = self.showResult(f(t), r)
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

      override def showResult(t: T, r: Res): String = {
        val prefix = s"$name predicate "
        val suffix = Try(pf(t)) match {
          case Success(_) => "."
          case Failure(e) => ": " + e.getMessage
        }
        prefix + suffix
      }
    }
}
