package eu.timepit.refined
package internal

/**
 * Helper class that allows the type `T` to be inferred from calls like
 * `[[RefType.refine]][P](t)`.
 *
 * See [[http://tpolecat.github.io/2015/07/30/infer.html]] for a detailed
 * explanation of this trick.
 */
final class RefineAux[F[_, _], P](rt: RefType[F]) {

  def apply[T, R](t: T)(implicit v: Validator[T, P, R]): Either[v.Res, F[T, P]] =
    v.validate(t) match {
      case Passed(_, _) => Right(rt.unsafeWrap(t))
      case rv => Left(rv)
    }
}
