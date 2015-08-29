package eu.timepit.refined

/**
 * Type class for validating values of type `T` according to a type-level
 * predicate `P`. The semantics of `P` are defined by the instance(s) of
 * this type class for `P`.
 */
trait Predicate[P, T, Out] extends Serializable {

  def isValid(t: T): Boolean

  def value: P = null.asInstanceOf[P]

  def validate(t: T): Option[String] =
    if (isValid(t)) None else Some(s"Predicate failed: .")

  def validate2(t: T): Out =
    if (isValid(t)) Passed(t, value).asInstanceOf[Out] else Failed(t, value).asInstanceOf[Out]

  val isConstant: Boolean = false
}
