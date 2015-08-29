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

  def apply[T, POut](t: T)(implicit p: Predicate[P, T, POut]): Either[POut, F[T, P]] =
    p.validate2(t) match {
      case Passed(_, _) => Right(rt.unsafeWrap(t))
      case _ => Left(p.validate2(t))
    }

  def apply2[T, POut](t: T)(implicit p: Predicate[P, T, POut], s: Show[POut]): Either[String, F[T, P]] =
    p.validate2(t) match {
      case Passed(_, _) => Right(rt.unsafeWrap(t))
      //case _ => Left(p.validate2(t))
      case _ => Left(s.show(p.validate2(t)))
    }
}
