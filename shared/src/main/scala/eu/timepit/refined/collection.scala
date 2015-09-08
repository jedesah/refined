package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.Result.{Passed, Failed}
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.collection._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.{GreaterEqual, LessEqual}
import shapeless.Witness

object collection extends CollectionValidators with CollectionInferenceRules {

  /**
   * Predicate that counts the number of elements in a `TraversableOnce`
   * which satisfy the predicate `PA` and passes the result to the numeric
   * predicate `PC`.
   */
  case class Count[PA, PC](pa: PA, pc: PC)

  /** Predicate that checks if a `TraversableOnce` is empty. */
  case class Empty()

  /**
   * Predicate that checks if the predicate `P` holds for all elements of a
   * `TraversableOnce`.
   */
  case class Forall[P](p: P)

  /**
   * Predicate that checks if the predicate `P` holds for the first element
   * of a `Traversable`.
   */
  case class Head[P](p: P)

  /**
   * Predicate that checks if the predicate `P` holds for the element at
   * index `N` of a sequence.
   */
  case class Index[N, P](n: N, p: P)

  /**
   * Predicate that checks if the predicate `P` holds for the last element
   * of a `Traversable`.
   */
  case class Last[P](p: P)

  /**
   * Predicate that checks if the size of a `TraversableOnce` satisfies the
   * predicate `P`.
   */
  case class Size[P](p: P)

  /**
   * Predicate that checks if a `TraversableOnce` contains a value
   * equal to `U`.
   */
  type Contains[U] = Exists[Equal[U]]

  /**
   * Predicate that checks if the predicate `P` holds for some elements of a
   * `TraversableOnce`.
   */
  type Exists[P] = Not[Forall[Not[P]]]

  /**
   * Predicate that checks if the size of a `TraversableOnce` is greater than
   * or equal to `N`.
   */
  type MinSize[N] = Size[GreaterEqual[N]]

  /**
   * Predicate that checks if the size of a `TraversableOnce` is less than
   * or equal to `N`.
   */
  type MaxSize[N] = Size[LessEqual[N]]

  /** Predicate that checks if a `TraversableOnce` is not empty. */
  type NonEmpty = Not[Empty]

  object Count {

    implicit def countValidator[A, PA, RA, PC, RC, T](
      implicit
      va: Validator[A, PA, RA], vc: Validator[Int, PC, RC], ev: T => TraversableOnce[A]
    ): Validator[T, Count[PA, PC], Count[List[va.Res], vc.Res]] =
      Validator.instance(t => {
        val ra = t.toList.map(va.validate)
        val rc = vc.validate(ra.count(_.isPassed))
        rc.mapBoth(_ => t, _ => Count(ra, rc))
      })

    implicit def countShow[A, PA, RA, PC, RC, T](
      implicit
      sa: Show[A, PA, RA], sc: Show[Int, PC, RC], ev: T => TraversableOnce[A]
    ): Show[T, Count[PA, PC], Count[List[sa.Res], sc.Res]] =
      new Show[T, Count[PA, PC], Count[List[sa.Res], sc.Res]] {
        override def showExpr(t: T): String =
          t.toList.map(sa.showExpr).mkString("count(", ", ", ")")

        override def showResult(r: Res): String = {
          val count = r.predicate.pa.count(_.isPassed)
          val prefix = r.describeWith(s"taking ${showExpr(r.value)} = $count")
          s"$prefix: ${sc.showResult(r.predicate.pc)}"
        }
      }
  }

  object Empty {

    implicit def emptyValidator[T](implicit ev: T => TraversableOnce[_]): Validator.Flat[T, Empty] =
      Validator.fromPredicate(_.isEmpty, Empty())

    implicit def emptyShow[T]: Show.Flat[T, Empty] =
      Show.instance(t => s"isEmpty($t)")
  }

  object Forall {

    implicit def forallValidator[A, P, R, T[a] <: TraversableOnce[a]](
      implicit
      v: Validator[A, P, R]
    ): Validator[T[A], Forall[P], Forall[List[v.Res]]] =
      Validator.instance(t => {
        val rt = t.toList.map(v.validate)
        Result.fromBoolean(rt.forall(_.isPassed), t, Forall(rt))
      })

    implicit def forallValidatorView[A, P, R, T](
      implicit
      v: Validator[A, P, R], ev: T => TraversableOnce[A]
    ): Validator[T, Forall[P], Forall[List[v.Res]]] =
      forallValidator.contramap(ev)

    implicit def forallShow[A, P, R, T[a] <: TraversableOnce[a]](
      implicit
      s: Show[A, P, R]
    ): Show[T[A], Forall[P], Forall[List[s.Res]]] =
      new Show[T[A], Forall[P], Forall[List[s.Res]]] {
        override def showExpr(t: T[A]): String = t.toList.map(s.showExpr).mkString("(", " && ", ")")

        override def showResult(r: Res): String =
          r match {
            case Passed(_, _) => s"Predicate ${showExpr(r.value)} passed."
            case Failed(_, _) => s"Predicate ${showExpr(r.value)} failed: " +
              r.predicate.p.filter(_.isFailed).map(s.showResult).mkString(" ")
          }
      }

    implicit def forallShowView[A, P, R, T](
      implicit
      s: Show[A, P, R], ev: T => TraversableOnce[A]
    ): Show[T, Forall[P], Forall[List[s.Res]]] =
      forallShow.contramap(ev)
  }

  object Size {

  }
}

private[refined] trait CollectionValidators {

  implicit def headValidator[A, P, R, T[a] <: Traversable[a]](
    implicit
    v: Validator[A, P, R]
  ): Validator[T[A], Head[P], Head[Option[v.Res]]] =
    Validator.instance(t => t.headOption match {
      case Some(a) =>
        val rv = v.validate(a)
        rv.mapBoth(_ => t, _ => Head(Some(rv)))
      case None => Failed(t, Head(None))
    })

  implicit def headValidatorView[A, P, R, T](
    implicit
    v: Validator[A, P, R], ev: T => Traversable[A]
  ): Validator[T, Head[P], Head[Option[v.Res]]] =
    headValidator.contramap(ev)

  implicit def indexValidator[A, P, R, T, N <: Int](
    implicit
    v: Validator[A, P, R],
    ev: T => PartialFunction[Int, A],
    wn: Witness.Aux[N]
  ): Validator[T, Index[N, P], Index[N, Option[v.Res]]] =
    Validator.instance(t => t.lift(wn.value) match {
      case Some(a) =>
        val rv = v.validate(a)
        rv.mapBoth(_ => t, _ => Index(wn.value, Some(rv)))
      case None => Failed(t, Index(wn.value, None))
    })

  implicit def lastValidator[A, P, R, T[a] <: Traversable[a]](
    implicit
    v: Validator[A, P, R]
  ): Validator[T[A], Last[P], Last[Option[v.Res]]] =
    Validator.instance(t => t.lastOption match {
      case Some(a) =>
        val rv = v.validate(a)
        rv.mapBoth(_ => t, _ => Last(Some(rv)))
      case None => Failed(t, Last(None))
    })

  implicit def lastValidatorView[A, P, R, T](
    implicit
    v: Validator[A, P, R], ev: T => Traversable[A]
  ): Validator[T, Last[P], Last[Option[v.Res]]] =
    lastValidator.contramap(ev)

  implicit def sizeValidator[T, P, R](
    implicit
    v: Validator[Int, P, R], ev: T => TraversableOnce[_]
  ): Validator[T, Size[P], Size[v.Res]] =
    Validator.instance(t => {
      val rv = v.validate(t.size)
      rv.mapBoth(_ => t, _ => Size(rv))
    })

  implicit def sizeShow[T, P, R](
    implicit
    s: Show[Int, P, R], ev: T => TraversableOnce[_]
  ): Show[T, Size[P], Size[s.Res]] =
    new Show[T, Size[P], Size[s.Res]] {
      override def showExpr(t: T): String = s.showExpr(t.size)

      override def showResult(r: Res): String = {
        val size = r.value.size
        val nested = s.showResult(r.predicate.p)

        def msg(result: String) =
          s"Predicate taking size(${r.value}) = $size $result: $nested"

        r.fold((_, _) => msg("passed"), (_, _) => msg("failed"))
      }
    }
}

private[refined] trait CollectionInferenceRules {

  implicit def existsInference[A, B](implicit p1: A ==> B): Exists[A] ==> Exists[B] =
    p1.adapt("existsInference(%s)")

  implicit def existsNonEmptyInference[P]: Exists[P] ==> NonEmpty =
    InferenceRule.alwaysValid("existsNonEmptyInference")

  implicit def headInference[A, B](implicit p1: A ==> B): Head[A] ==> Head[B] =
    p1.adapt("headInference(%s)")

  implicit def headExistsInference[P]: Head[P] ==> Exists[P] =
    InferenceRule.alwaysValid("headExistsInference")

  implicit def indexInference[N, A, B](implicit p1: A ==> B): Index[N, A] ==> Index[N, B] =
    p1.adapt("indexInference(%s)")

  implicit def indexExistsInference[N, P]: Index[N, P] ==> Exists[P] =
    InferenceRule.alwaysValid("indexExistsInference")

  implicit def lastInference[A, B](implicit p1: A ==> B): Last[A] ==> Last[B] =
    p1.adapt("lastInference(%s)")

  implicit def lastExistsInference[P]: Last[P] ==> Exists[P] =
    InferenceRule.alwaysValid("lastExistsInference")

  implicit def sizeInference[A, B](implicit p1: A ==> B): Size[A] ==> Size[B] =
    p1.adapt("sizeInference(%s)")
}
