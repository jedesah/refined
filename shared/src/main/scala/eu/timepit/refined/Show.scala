package eu.timepit.refined

import eu.timepit.refined.Result.{Failed, Passed}

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

  def apply[T, P]: ShowAux[T, P] = new ShowAux

  final class ShowAux[T, P] {
    def get[R](implicit s: Show[T, P, R]) = s
  }

  def instance[T, P, R](f: T => String): Show[T, P, R] =
    new Show[T, P, R] {
      override def showExpr(t: T): String = f(t)
    }
}
