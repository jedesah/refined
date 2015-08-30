package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.Result.{Failed, Passed}
import eu.timepit.refined.boolean._
import shapeless.ops.hlist.ToList
import shapeless.{::, HList, HNil}

object boolean extends BooleanValidators with BooleanInferenceRules0 {

  /** Constant predicate that is always `true`. */
  case class True()

  /** Constant predicate that is always `false`. */
  case class False()

  /** Negation of the predicate `P`. */
  case class Not[P](p: P)

  /** Conjunction of the predicates `A` and `B`. */
  case class And[A, B](a: A, b: B)

  /** Disjunction of the predicates `A` and `B`. */
  case class Or[A, B](a: A, b: B)

  /** Exclusive disjunction of the predicates `A` and `B`. */
  case class Xor[A, B](a: A, b: B)

  /** Conjunction of all predicates in `PS`. */
  case class AllOf[PS](ps: PS)

  /** Disjunction of all predicates in `PS`. */
  case class AnyOf[PS](ps: PS)

  /** Exclusive disjunction of all predicates in `PS`. */
  case class OneOf[PS](ps: PS)
}

private[refined] trait BooleanValidators {

  implicit def trueValidator[T]: Validator.Flat[T, True] =
    Validator.constant(t => Passed(t, True()))

  implicit def falseValidator[T]: Validator.Flat[T, False] =
    Validator.constant(t => Failed(t, False()))

  implicit def notValidator[T, P, R](implicit v: Validator[T, P, R]): Validator[T, Not[P], Not[v.Res]] =
    new Validator[T, Not[P], Not[v.Res]] {
      override def validate(t: T): Res =
        v.validate(t) match {
          case r @ Passed(_, _) => Failed(t, Not(r))
          case r @ Failed(_, _) => Passed(t, Not(r))
        }

      override val isConstant: Boolean = v.isConstant
    }

  implicit def andValidator[T, A, B, RA, RB](
    implicit
    va: Validator[T, A, RA], vb: Validator[T, B, RB]
  ): Validator[T, A And B, va.Res And vb.Res] =
    new Validator[T, A And B, va.Res And vb.Res] {
      override def validate(t: T): Res =
        (va.validate(t), vb.validate(t)) match {
          case (ra @ Passed(_, _), rb @ Passed(_, _)) => Passed(t, And(ra, rb))
          case (ra, rb) => Failed(t, And(ra, rb))
        }

      override val isConstant: Boolean = va.isConstant && vb.isConstant
    }

  implicit def orValidator[T, A, B, RA, RB](
    implicit
    va: Validator[T, A, RA], vb: Validator[T, B, RB]
  ): Validator[T, A Or B, va.Res Or vb.Res] =
    new Validator[T, A Or B, va.Res Or vb.Res] {
      override def validate(t: T): Res =
        (va.validate(t), vb.validate(t)) match {
          case (ra @ Failed(_, _), rb @ Failed(_, _)) => Failed(t, Or(ra, rb))
          case (ra, rb) => Passed(t, Or(ra, rb))
        }

      override val isConstant: Boolean = va.isConstant && vb.isConstant
    }

  implicit def xorValidator[T, A, B, RA, RB](
    implicit
    va: Validator[T, A, RA], vb: Validator[T, B, RB]
  ): Validator[T, A Xor B, va.Res Xor vb.Res] =
    new Validator[T, A Xor B, va.Res Xor vb.Res] {
      override def validate(t: T): Res =
        (va.validate(t), vb.validate(t)) match {
          case (ra @ Failed(_, _), rb @ Failed(_, _)) => Failed(t, Xor(ra, rb))
          case (ra @ Passed(_, _), rb @ Passed(_, _)) => Failed(t, Xor(ra, rb))
          case (ra, rb) => Passed(t, Xor(ra, rb))
        }

      override val isConstant: Boolean = va.isConstant && vb.isConstant
    }

  implicit def allOfHNilValidator[T]: Validator.Flat[T, AllOf[HNil]] =
    Validator.constant(t => Passed(t, AllOf(HList())))

  implicit def allOfHConsValidator[T, PH, PT <: HList, RH, RT <: HList](
    implicit
    vh: Validator[T, PH, RH], vt: Validator[T, AllOf[PT], AllOf[RT]]
  ): Validator[T, AllOf[PH :: PT], AllOf[vh.Res :: RT]] =
    new Validator[T, AllOf[PH :: PT], AllOf[vh.Res :: RT]] {
      override def validate(t: T): Res =
        (vh.validate(t), vt.validate(t)) match {
          case (rh @ Passed(_, _), rt @ Passed(_, _)) => Passed(t, AllOf(rh :: rt.predicate.ps))
          case (rh, rt) => Failed(t, AllOf(rh :: rt.predicate.ps))
        }

      override val isConstant: Boolean = vh.isConstant && vt.isConstant
    }

  implicit def anyOfHNilValidator[T]: Validator.Flat[T, AnyOf[HNil]] =
    Validator.constant(t => Failed(t, AnyOf(HList())))

  implicit def anyOfHConsValidator[T, PH, PT <: HList, RH, RT <: HList](
    implicit
    vh: Validator[T, PH, RH], vt: Validator[T, AnyOf[PT], AnyOf[RT]]
  ): Validator[T, AnyOf[PH :: PT], AnyOf[vh.Res :: RT]] =
    new Validator[T, AnyOf[PH :: PT], AnyOf[vh.Res :: RT]] {
      override def validate(t: T): Res =
        (vh.validate(t), vt.validate(t)) match {
          case (rh @ Passed(_, _), rt) => Passed(t, AnyOf(rh :: rt.predicate.ps))
          case (rh, rt @ Passed(_, _)) => Passed(t, AnyOf(rh :: rt.predicate.ps))
          case (rh, rt) => Failed(t, AnyOf(rh :: rt.predicate.ps))
        }

      override val isConstant: Boolean = vh.isConstant && vt.isConstant
    }

  implicit def oneOfHNilValidator[T]: Validator.Flat[T, OneOf[HNil]] =
    Validator.constant(t => Failed(t, OneOf(HList())))

  implicit def oneOfHConsValidator[T, PH, PT <: HList, RH, RT <: HList](
    implicit
    vh: Validator[T, PH, RH], vt: Validator[T, OneOf[PT], OneOf[RT]], toList: ToList[RT, Result[_, _]]
  ): Validator[T, OneOf[PH :: PT], OneOf[vh.Res :: RT]] =
    new Validator[T, OneOf[PH :: PT], OneOf[vh.Res :: RT]] {
      override def validate(t: T): Res = {
        val rt = vt.validate(t).predicate.ps
        val passedCount = toList(rt).count(_.isPassed)

        vh.validate(t) match {
          case rh @ Passed(_, _) if passedCount == 0 => Passed(t, OneOf(rh :: rt))
          case rh @ Failed(_, _) if passedCount == 1 => Passed(t, OneOf(rh :: rt))
          case rh => Failed(t, OneOf(rh :: rt))
        }
      }

      override val isConstant: Boolean = vh.isConstant && vt.isConstant
    }
}

private[refined] trait BooleanShows {

  implicit def andShow[T, A, B](implicit sa: Show[T, A], sb: Show[T, B]): Show[T, A And B] =
    new Show[T, A And B] {
      override def show(t: T): String = s"(${sa.show(t)} && ${sb.show(t)}})"
    }
}

private[refined] trait BooleanInferenceRules0 extends BooleanInferenceRules1 {

  implicit def minimalTautology[A]: A ==> A =
    InferenceRule.alwaysValid("minimalTautology")

  implicit def doubleNegationElimination[A, B](implicit p1: A ==> B): Not[Not[A]] ==> B =
    p1.adapt("doubleNegationElimination(%s)")

  implicit def doubleNegationIntroduction[A, B](implicit p1: A ==> B): A ==> Not[Not[B]] =
    p1.adapt("doubleNegationIntroduction(%s)")

  implicit def conjunctionAssociativity[A, B, C]: ((A And B) And C) ==> (A And (B And C)) =
    InferenceRule.alwaysValid("conjunctionAssociativity")

  implicit def conjunctionCommutativity[A, B]: (A And B) ==> (B And A) =
    InferenceRule.alwaysValid("conjunctionCommutativity")

  implicit def conjunctionEliminationR[A, B, C](implicit p1: B ==> C): (A And B) ==> C =
    p1.adapt("conjunctionEliminationR(%s)")

  implicit def disjunctionAssociativity[A, B, C]: ((A Or B) Or C) ==> (A Or (B Or C)) =
    InferenceRule.alwaysValid("disjunctionAssociativity")

  implicit def disjunctionCommutativity[A, B]: (A Or B) ==> (B Or A) =
    InferenceRule.alwaysValid("disjunctionCommutativity")

  implicit def disjunctionIntroductionL[A, B]: A ==> (A Or B) =
    InferenceRule.alwaysValid("disjunctionIntroductionL")

  implicit def disjunctionIntroductionR[A, B]: B ==> (A Or B) =
    InferenceRule.alwaysValid("disjunctionIntroductionR")

  implicit def deMorgansLaw1[A, B]: Not[A And B] ==> (Not[A] Or Not[B]) =
    InferenceRule.alwaysValid("deMorgansLaw1")

  implicit def deMorgansLaw2[A, B]: Not[A Or B] ==> (Not[A] And Not[B]) =
    InferenceRule.alwaysValid("deMorgansLaw2")

  implicit def xorCommutativity[A, B]: (A Xor B) ==> (B Xor A) =
    InferenceRule.alwaysValid("xorCommutativity")

  implicit def modusTollens[A, B](implicit p1: A ==> B): Not[B] ==> Not[A] =
    p1.adapt("modusTollens(%s)")
}

private[refined] trait BooleanInferenceRules1 {

  implicit def conjunctionEliminationL[A, B, C](implicit p1: A ==> C): (A And B) ==> C =
    p1.adapt("conjunctionEliminationL(%s)")

  implicit def hypotheticalSyllogism[A, B, C](implicit p1: A ==> B, p2: B ==> C): A ==> C =
    InferenceRule.combine(p1, p2, "hypotheticalSyllogism(%s, %s)")
}
