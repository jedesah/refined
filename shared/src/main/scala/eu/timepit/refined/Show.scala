package eu.timepit.refined

trait Show[P] {
  def show(p: P): String
}
