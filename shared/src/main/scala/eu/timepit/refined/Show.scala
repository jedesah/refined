package eu.timepit.refined

trait Show[T, P, R] extends Serializable {

  final type Res = Result[T, R]

  def show(t: T): String

  def showResult(r: Res): String = show(r.value)
}

object Show {

  type Flat[T, P] = Show[T, P, P]

  def apply[T, P]: ShowAux[T, P] = new ShowAux

  final class ShowAux[T, P] {
    def get[R](implicit s: Show[T, P, R]) = s
  }

  def instance[T, P, R](f: T => String): Show[T, P, R] =
    new Show[T, P, R] {
      override def show(t: T): String = f(t)
    }
}
