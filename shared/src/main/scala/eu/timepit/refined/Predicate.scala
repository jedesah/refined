package eu.timepit.refined

import scala.util.{Failure, Success, Try}

/**
 * Type class for validating values of type `T` according to a type-level
 * predicate `P`. The semantics of `P` are defined by the instance(s) of
 * this type class for `P`.
 */
trait Predicate[P, T, Out] extends Serializable { self =>

  /** Checks if `t` satisfies the predicate `P`. */
  def isValid(t: T): Boolean

  // TODO: Find a better name and make this abstract.
  def value: P = null.asInstanceOf[P]

  /**
   * Returns `None` if `t` satisfies the predicate `P`, or an error message
   * contained in `Some` otherwise.
   */
  def validate(t: T): Option[String] =
    if (isValid(t)) None else Some(s"Predicate failed: .")

  def validate2(t: T): Out =
    if (isValid(t)) Passed(t, value).asInstanceOf[Out] else Failed(t, value).asInstanceOf[Out]

  /**
   * Denotes whether this [[Predicate]] is constant (which is false by
   * default). A constant [[Predicate]] ignores the argument passed to
   * [[isValid]].
   */
  val isConstant: Boolean = false

  /** Checks if `t` does not satisfy the predicate `P`. */
  final def notValid(t: T): Boolean =
    !isValid(t)

  /**
   * Returns the result of [[isValid]] in a `List`. Can be overridden to
   * accumulate the results of sub-predicates.
   */
  def accumulateIsValid(t: T): List[Boolean] =
    List(isValid(t))

  /**
   * Returns the result of [[show]] in a `List`. Can be overridden to
   * accumulate the string representations of sub-predicates.
   */
  /*def accumulateShow(t: T): List[String] =
    List(show(t))
*/

  /*
  private[refined] def contramap[U](f: U => T): Predicate[P, U] =
    new Predicate[P, U] {
      def isValid(u: U): Boolean = self.isValid(f(u))
      def show(u: U): String = self.show(f(u))
      override def validate(u: U): Option[String] = self.validate(f(u))
      override val isConstant: Boolean = self.isConstant
      override def accumulateIsValid(u: U): List[Boolean] = self.accumulateIsValid(f(u))
      override def accumulateShow(u: U): List[String] = self.accumulateShow(f(u))
    }
    */
}

object Predicate {

  //type Aux[P, T, Out1] = Predicate[P, T] { type Out = Out1 }

  def apply[P, T, O](implicit p: Predicate[P, T, O]): Predicate[P, T, O] = p

  /** Constructs a [[Predicate]] from its parameters. */
  /*
  def instance[P, T, O](isValidF: T => Boolean, showF: T => String, constant: Boolean = false): Predicate[P, T] =
    new Predicate[P, T] {
      def isValid(t: T): Boolean = isValidF(t)
      def show(t: T): String = showF(t)
      override val isConstant: Boolean = constant
    }
    */

  /*
  def instance2[P, T, O](isValidF: T => Boolean, valueV: P, showF: T => String, constant: Boolean = false): Predicate[P, T] =
    new Predicate[P, T] {
      def isValid(t: T): Boolean = isValidF(t)
      override def value: P = valueV
      def show(t: T): String = showF(t)
      override val isConstant: Boolean = constant
    }
    */

  /** Constructs a constant [[Predicate]] from its parameters. */
  //def constant[P, T](isValidV: Boolean, showV: String): Predicate[P, T] =
  //  instance(_ => isValidV, _ => showV, constant = true)

  /*
  /** Returns a [[Predicate]] that ignores its inputs and always yields `true`. */
  def alwaysValid[P, T]: Predicate[P, T] =
    constant(isValidV = true, "true")

  /** Returns a [[Predicate]] that ignores its inputs and always yields `false`. */
  def alwaysInvalid[P, T]: Predicate[P, T] =
    constant(isValidV = false, "false")
  */
}
