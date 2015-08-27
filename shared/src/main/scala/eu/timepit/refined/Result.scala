package eu.timepit.refined

sealed trait Result[A] { def a: A }
case class Passed[A](a: A) extends Result[A]
case class Failed[A](a: A) extends Result[A]

object Result {
  implicit def resultShow[A](implicit s: Show[A]): Show[Result[A]] =
    new Show[Result[A]] {
      override def show(p: Result[A]): String =
        p match {
          case Passed(a) => "passed: " + s.show(a)
          case Failed(a) => "failed: " + s.show(a)
        }
    }
}