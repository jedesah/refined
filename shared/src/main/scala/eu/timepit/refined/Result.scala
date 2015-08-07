package eu.timepit.refined

sealed trait Result[A]
case class Passed[A](a: A) extends Result[A]
case class Failed[A](a: A) extends Result[A]
