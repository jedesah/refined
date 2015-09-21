package eu.timepit.refined.api

trait Show[T, P] extends Serializable {

  type R

  final type Res = Result[R]

  def showExpr(t: T): String
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
}
