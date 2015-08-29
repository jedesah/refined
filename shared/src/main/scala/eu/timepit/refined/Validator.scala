package eu.timepit.refined

trait Validator[T, P, R] extends Serializable {

  type Res = Result[T, R]

  def validate(t: T): Res

  def isConstant: Boolean = false
}

object Validator {

  type Flat[T, P] = Validator[T, P, P]

  def apply[T, P]: ValidatorAux[T, P] = new ValidatorAux[T, P]

  class ValidatorAux[T, P] {
    def get[R](implicit v: Validator[T, P, R]): Validator[T, P, R] = v
  }

  def instance[T, P, R](validateFun: T => Result[T, R], constant: Boolean = false): Validator[T, P, R] =
    new Validator[T, P, R] {
      override def validate(t: T): Result[T, R] = validateFun(t)
      override def isConstant: Boolean = constant
    }

  def constant[T, P, R](validateFun: T => Result[T, R]): Validator[T, P, R] =
    instance(validateFun, constant = true)

  def fromPredicate[T, P](f: T => Boolean, p: P): Validator.Flat[T, P] =
    instance(t => if (f(t)) Passed(t, p) else Failed(t, p))
}
