package eu.timepit.refined

trait Show[T, P] extends Serializable {
  def show(t: T): String
}

object Show {

}
