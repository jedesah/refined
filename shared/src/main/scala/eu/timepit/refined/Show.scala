package eu.timepit.refined

trait Show[A] {
  def show(a: A): String
}
