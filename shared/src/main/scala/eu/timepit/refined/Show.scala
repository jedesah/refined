package eu.timepit.refined

trait Show[T, P, R] extends Serializable {

  final type Res = Result[T, R]

  def show(t: T): String

  def showResult(r: Res): String = show(r.value)
}

object Show {

}
