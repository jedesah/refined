package eu.timepit.refined
package internal

import eu.timepit.refined.Result.Passed

import scala.reflect.macros.blackbox

/**
 * Helper class that allows the type `T` to be inferred from calls like
 * `[[RefType.refineM]][P](t)`.
 *
 * See [[http://tpolecat.github.io/2015/07/30/infer.html]] for a detailed
 * explanation of this trick.
 */
final class RefineMAux[F[_, _], P] {

  def apply[T, R](t: T)(implicit v: Validator[T, P, R], rt: RefType[F]): F[T, P] = macro RefineMAux.macroImpl[F, T, P, R]
}

object RefineMAux {

  def macroImpl[F[_, _], T: c.WeakTypeTag, P: c.WeakTypeTag, R](c: blackbox.Context)(t: c.Expr[T])(
    v: c.Expr[Validator[T, P, R]], rt: c.Expr[RefType[F]]
  ): c.Expr[F[T, P]] = {
    import c.universe._

    val validator = MacroUtils.eval(c)(v)

    val tValue: T = t.tree match {
      case Literal(Constant(value)) => value.asInstanceOf[T]
      case _ if validator.isConstant => null.asInstanceOf[T]
      case _ => c.abort(
        c.enclosingPosition,
        "compile-time refinement only works with literals or constant predicates"
      )
    }

    validator.validate(tValue) match {
      case Passed(_, _) =>
        val refType = MacroUtils.eval(c)(rt)
        refType.unsafeWrapM(c)(t)
      case rv => c.abort(c.enclosingPosition, rv.toString) // TODO: use Show
    }
  }
}
