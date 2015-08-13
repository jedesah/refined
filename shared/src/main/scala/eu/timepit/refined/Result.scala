package eu.timepit.refined

sealed trait Result[A] { def a: A }
case class Passed[A](a: A) extends Result[A]
case class Failed[A](a: A) extends Result[A]
