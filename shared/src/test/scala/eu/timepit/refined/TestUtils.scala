package eu.timepit.refined

object TestUtils {

  def isValid[P]: IsValidAux[P] = new IsValidAux

  class IsValidAux[P] {
    def apply[T, R](t: T)(implicit v: Validator[T, P, R]): Boolean =
      v.isValid(t)
  }

  def validate[P]: ValidateAux[P] = new ValidateAux

  class ValidateAux[P] {
    def apply[T, R](t: T)(implicit v: Validator[T, P, R]): v.Res =
      v.validate(t)
  }

  def showExpr[P]: ShowExprAux[P] = new ShowExprAux

  class ShowExprAux[P] {
    def apply[T, R](t: T)(implicit s: Show[T, P, R]): String =
      s.showExpr(t)
  }

  def showResult[P]: ShowResultAux[P] = new ShowResultAux

  class ShowResultAux[P] {
    def apply[T, R](t: T)(implicit v: Validator[T, P, R], s: Show[T, P, R]): String =
      s.showResult(v.validate(t))
  }
}
