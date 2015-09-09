package eu.timepit.refined

/**
 * Type class for validating values of type `T` according to a type-level
 * predicate `P`. The semantics of `P` are defined by the instance(s) of
 * this type class for `P`.
 */
trait Validator[T, P, R] extends Serializable { self =>

  final type Res = Result[R]

  def validate(t: T): Res

  /**
   * Denotes whether this [[Validator]] is constant (which is false by
   * default). A constant [[Validator]] ignores the argument passed to
   * [[validate]].
   */
  def isConstant: Boolean = false

  /** Checks if `t` satisfies the predicate `P`. */
  final def isValid(t: T): Boolean =
    validate(t).isPassed

  /** Checks if `t` does not satisfy the predicate `P`. */
  final def notValid(t: T): Boolean =
    validate(t).isFailed

  private[refined] def contramap[U](f: U => T): Validator[U, P, R] =
    new Validator[U, P, R] {
      override def validate(u: U): Res = self.validate(f(u))
      override def isConstant: Boolean = self.isConstant
    }
}

object Validator {

  type Flat[T, P] = Validator[T, P, P]

  /** Constructs a [[Validator]] from its parameters. */
  def instance[T, P, R](validateFun: T => Result[R], constant: Boolean = false): Validator[T, P, R] =
    new Validator[T, P, R] {
      override def validate(t: T): Result[R] = validateFun(t)
      override val isConstant: Boolean = constant
    }

  /** Constructs a constant [[Validator]] from its parameter. */
  def constant[T, P, R](validateFun: T => Result[R]): Validator[T, P, R] =
    instance(validateFun, constant = true)

  /**
   * Constructs a [[Validator]] from the predicate `f`. All values of type
   * `T` for which `f` returns `true` are considered valid according to `P`.
   */
  def fromPredicate[T, P](f: T => Boolean, p: P): Validator.Flat[T, P] =
    instance(t => Result.fromBoolean(f(t), p))

  /**
   * Constructs a [[Validator]] from the partial function `pf`. All values of
   * type `T` for which `pf` does not throw an exception are considered valid
   * according to `P`.
   */
  def fromPartial[T, U, P](pf: T => U, p: P): Validator.Flat[T, P] =
    fromPredicate(t => scala.util.Try(pf(t)).isSuccess, p)
}
