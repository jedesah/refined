package eu.timepit.refined

import scala.util.Try

trait Validator[T, P, R] extends Serializable { self =>

  final type Res = Result[T, R]

  def validate(t: T): Res

  def isConstant: Boolean = false

  final def isValid(t: T): Boolean =
    validate(t).isPassed

  final def notValid(t: T): Boolean =
    validate(t).isFailed

  private[refined] def contramap[U](f: U => T): Validator[U, P, R] =
    new Validator[U, P, R] {
      override def validate(u: U): Res = self.validate(f(u)).mapFst(_ => u)
      override def isConstant: Boolean = self.isConstant
    }
}

object Validator {

  type Flat[T, P] = Validator[T, P, P]

  def apply[T, P]: ValidatorAux[T, P] = new ValidatorAux[T, P]

  class ValidatorAux[T, P] {
    def get[R](implicit v: Validator[T, P, R]): Validator[T, P, R] = v
  }

  /** Constructs a [[Validator]] from its parameters. */
  def instance[T, P, R](validateFun: T => Result[T, R], constant: Boolean = false): Validator[T, P, R] =
    new Validator[T, P, R] {
      override def validate(t: T): Result[T, R] = validateFun(t)
      override val isConstant: Boolean = constant
    }

  /** Constructs a constant [[Validator]] from its parameter. */
  def constant[T, P, R](validateFun: T => Result[T, R]): Validator[T, P, R] =
    instance(validateFun, constant = true)

  def fromPredicate[T, P](f: T => Boolean, p: P): Validator.Flat[T, P] =
    instance(t => if (f(t)) Passed(t, p) else Failed(t, p))

  /**
   * Constructs a [[Validator]] from the partial function `pf`. All values of
   * type `T` for which `pf` does not throw an exception are considered valid
   * according to `P`.
   */
  def fromPartial[T, U, P](pf: T => U, p: P): Validator.Flat[T, P] =
    fromPredicate(t => Try(pf(t)).isSuccess, p)
}
