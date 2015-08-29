package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
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
}

private[refined] trait CollectionValidators {

  /*
  implicit def countPredicate[PA, PC, A, T](implicit pa: Predicate[PA, A], pc: Predicate[PC, Int], ev: T => TraversableOnce[A]): Predicate[Count[PA, PC], T] =
    new Predicate[Count[PA, PC], T] {
      def isValid(t: T): Boolean = pc.isValid(count(t))
      override def value: Count[PA, PC] = Count(pa.value, pc.value)
      def show(t: T): String = pc.show(count(t))

      override def validate(t: T): Option[String] = {
        val c = count(t)
        val expr = t.toSeq.map(pa.show).mkString("count(", ", ", ")")
        pc.validate(c).map(msg => s"Predicate taking $expr = $c failed: $msg")
      }

      private def count(t: T): Int = t.count(pa.isValid)
    }
*/

  implicit def emptyValidator[T](implicit ev: T => TraversableOnce[_]): Validator.Flat[T, Empty] =
    Validator.fromPredicate(_.isEmpty, Empty())

  /*
  implicit def forallPredicate[P, A, T[A] <: TraversableOnce[A]](implicit p: Predicate[P, A]): Predicate[Forall[P], T[A]] =
    Predicate.instance2(
      _.forall(p.isValid),
      Forall(p.value),
      _.toSeq.map(p.show).mkString("(", " && ", ")")
    )

  implicit def forallPredicateView[P, A, T](implicit p: Predicate[P, A], ev: T => TraversableOnce[A]): Predicate[Forall[P], T] =
    forallPredicate.contramap(ev)

  implicit def headPredicate[P, A, T[A] <: Traversable[A]](implicit p: Predicate[P, A]): Predicate[Head[P], T[A]] =
    singleElemPredicate(_.headOption, (t: T[A], a: A) => s"head($t) = $a")

  implicit def headPredicateView[P, A, T](implicit p: Predicate[P, A], ev: T => Traversable[A]): Predicate[Head[P], T] =
    headPredicate.contramap(ev)

  implicit def indexPredicate[N <: Int, P, A, T](implicit p: Predicate[P, A], ev: T => PartialFunction[Int, A], wn: Witness.Aux[N]): Predicate[Index[N, P], T] =
    singleElemPredicate(_.lift(wn.value), (t: T, a: A) => s"index($t, ${wn.value}) = $a")

  implicit def lastPredicate[P, A, T[A] <: Traversable[A]](implicit p: Predicate[P, A]): Predicate[Last[P], T[A]] =
    singleElemPredicate(_.lastOption, (t: T[A], a: A) => s"last($t) = $a")

  implicit def lastPredicateView[P, A, T](implicit p: Predicate[P, A], ev: T => Traversable[A]): Predicate[Last[P], T] =
    lastPredicate.contramap(ev)

  private def singleElemPredicate[PA, PT, A, T](get: T => Option[A], describe: (T, A) => String)(implicit p: Predicate[PA, A]): Predicate[PT, T] =
    new Predicate[PT, T] {
      def isValid(t: T): Boolean = get(t).fold(false)(p.isValid)
      def show(t: T): String = get(t).fold("no element")(p.show)

      override def validate(t: T): Option[String] =
        get(t) match {
          case Some(a) =>
            p.validate(a).map(msg => s"Predicate taking ${describe(t, a)} failed: $msg")
          case None =>
            Some("Predicate failed: empty collection.")
        }
    }
*/

  implicit def sizeValidator[T, P, R](
    implicit
    v: Validator[Int, P, R], ev: T => TraversableOnce[_]
  ): Validator[T, Size[P], Size[v.Res]] =
    Validator.instance(t => {
      val rv = v.validate(t.size)
      rv.mapBoth(_ => t, _ => Size(rv))
    })
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
