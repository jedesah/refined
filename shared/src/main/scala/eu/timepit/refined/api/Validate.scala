package eu.timepit.refined.api

trait Validate[T, P] extends Serializable {

  type R

  final type Res = Result[R]

  def validate(t: T): Res
}

object Validate {

  type Aux[T, P, R0] = Validate[T, P] { type R = R0 }

  type Flat[T, P] = Aux[T, P, P]

  def apply[T, P](implicit v: Validate[T, P]): Aux[T, P, v.R] = v

  /** Constructs a [[Validate]] from its parameters. */
  def instance[T, P, R0](f: T => Result[R0]): Aux[T, P, R0] =
    new Validate[T, P] {
      override type R = R0
      override def validate(t: T): Res = f(t)
    }

  /**
   * Constructs a [[Validate]] from the predicate `f`. All values of type `T`
   * for which `f` returns `true` are considered valid according to `P`.
   */
  def fromPredicate[T, P](f: T => Boolean, p: P): Flat[T, P] =
    instance(t => Result.fromBoolean(f(t), p))

  /**
   * Constructs a [[Validate]] from the partial function `pf`. All values of
   * type `T` for which `pf` does not throw an exception are considered valid
   * according to `P`.
   */
  def fromPartial[T, U, P](pf: T => U, p: P): Flat[T, P] =
    fromPredicate(t => scala.util.Try(pf(t)).isSuccess, p)
}
