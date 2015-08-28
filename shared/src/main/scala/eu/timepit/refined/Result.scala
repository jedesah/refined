package eu.timepit.refined

sealed trait Result[P] {
  def p: P
}

case class Passed[P](p: P) extends Result[P]
case class Failed[P](p: P) extends Result[P]

object Result {
  implicit def resultShow[A](implicit sa: Show[A]): Show[Result[A]] =
    new Show[Result[A]] {
      override def show(p: Result[A]): String =
        p match {
          case Passed(a) => "passed: " + sa.show(a)
          case Failed(a) => "failed: " + sa.show(a)
        }
    }
}