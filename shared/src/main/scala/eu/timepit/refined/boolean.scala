package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.Result.{Failed, Passed}
import eu.timepit.refined.boolean._
import shapeless.ops.hlist.ToList
import shapeless.{::, HList, HNil}

object boolean extends BooleanInferenceRules0 {

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

  object True {

    implicit def trueValidator[T]: Validator.Flat[T, True] =
      Validator.constant(t => Passed(t, True()))

    implicit def trueShow[T]: Show.Flat[T, True] =
      Show.instance(_ => "true")
  }

  object False {

    implicit def falseValidator[T]: Validator.Flat[T, False] =
      Validator.constant(t => Failed(t, False()))

    implicit def falseShow[T]: Show.Flat[T, False] =
      Show.instance(_ => "false")
  }

  object Not {

    implicit def notValidator[T, P, R](implicit v: Validator[T, P, R]): Validator[T, Not[P], Not[v.Res]] =
      Validator.instance(t => {
        val p = Not(v.validate(t))
        Result.fromBoolean(p.p.isFailed, t, p)
      }, v.isConstant)

    implicit def notShow[T, P, R](implicit s: Show[T, P, R]): Show[T, Not[P], Not[s.Res]] =
      new Show[T, Not[P], Not[s.Res]] {
        override def showExpr(t: T): String = s"!${s.showExpr(t)}"

        override def showResult(r: Res): String =
          r match {
            case Passed(t, _) => s"Predicate ${s.showExpr(t)} did not pass."
            case Failed(t, _) => s"Predicate ${s.showExpr(t)} did not fail."
          }
      }
  }

  object And {

    implicit def andValidator[T, A, B, RA, RB](
      implicit
      va: Validator[T, A, RA], vb: Validator[T, B, RB]
    ): Validator[T, A And B, va.Res And vb.Res] =
      Validator.instance(t => {
        val p = And(va.validate(t), vb.validate(t))
        Result.fromBoolean(p.a.isPassed && p.b.isPassed, t, p)
      }, va.isConstant && vb.isConstant)

    implicit def andShow[T, A, B, RA, RB](
      implicit
      sa: Show[T, A, RA], sb: Show[T, B, RB]
    ): Show[T, A And B, sa.Res And sb.Res] =
      new Show[T, A And B, sa.Res And sb.Res] {
        override def showExpr(t: T): String = s"(${sa.showExpr(t)} && ${sb.showExpr(t)})"

        override def showResult(r: Res): String = {
          val expr = showExpr(r.value)
          val (ra, rb) = (r.predicate.a, r.predicate.b)
          (ra, rb) match {
            case (Passed(_, _), Passed(_, _)) => s"Both predicates of $expr passed."
            case (Passed(_, _), Failed(_, _)) => s"Right predicate of $expr failed: ${sb.showResult(rb)}"
            case (Failed(_, _), Passed(_, _)) => s"Left predicate of $expr failed: ${sa.showResult(ra)}"
            case (Failed(_, _), Failed(_, _)) => s"Both predicates of $expr failed. " +
              s"Left: ${sa.showResult(ra)} Right: ${sb.showResult(rb)}"
          }
        }
      }
  }

  object Or {

    implicit def orValidator[T, A, B, RA, RB](
      implicit
      va: Validator[T, A, RA], vb: Validator[T, B, RB]
    ): Validator[T, A Or B, va.Res Or vb.Res] =
      Validator.instance(t => {
        val p = Or(va.validate(t), vb.validate(t))
        Result.fromBoolean(p.a.isPassed || p.b.isPassed, t, p)
      }, va.isConstant && vb.isConstant)

    implicit def orShow[T, A, B, RA, RB](
      implicit
      sa: Show[T, A, RA], sb: Show[T, B, RB]
    ): Show[T, A Or B, sa.Res Or sb.Res] =
      new Show[T, A Or B, sa.Res Or sb.Res] {
        override def showExpr(t: T): String = s"(${sa.showExpr(t)} || ${sb.showExpr(t)})"

        override def showResult(r: Res): String = {
          val expr = showExpr(r.value)
          val (ra, rb) = (r.predicate.a, r.predicate.b)
          (ra, rb) match {
            case (Passed(_, _), Passed(_, _)) => s"Both predicates of $expr passed."
            case (Passed(_, _), Failed(_, _)) => s"Left predicate of $expr passed."
            case (Failed(_, _), Passed(_, _)) => s"Right predicate of $expr passed."
            case (Failed(_, _), Failed(_, _)) => s"Both predicates of $expr failed. " +
              s"Left: ${sa.showResult(ra)} Right: ${sb.showResult(rb)}"
          }
        }
      }
  }

  object Xor {

    implicit def xorValidator[T, A, B, RA, RB](
      implicit
      va: Validator[T, A, RA], vb: Validator[T, B, RB]
    ): Validator[T, A Xor B, va.Res Xor vb.Res] =
      Validator.instance(t => {
        val p = Xor(va.validate(t), vb.validate(t))
        Result.fromBoolean(p.a.isPassed ^ p.b.isPassed, t, p)
      }, va.isConstant && vb.isConstant)

    implicit def xorShow[T, A, B, RA, RB](
      implicit
      sa: Show[T, A, RA], sb: Show[T, B, RB]
    ): Show[T, A Xor B, sa.Res Xor sb.Res] =
      new Show[T, A Xor B, sa.Res Xor sb.Res] {
        override def showExpr(t: T): String = s"(${sa.showExpr(t)} ^ ${sb.showExpr(t)})"

        override def showResult(r: Res): String = {
          val expr = showExpr(r.value)
          val (ra, rb) = (r.predicate.a, r.predicate.b)
          (ra, rb) match {
            case (Passed(_, _), Passed(_, _)) => s"Both predicates of $expr passed."
            case (Passed(_, _), Failed(_, _)) => s"Left predicate of $expr passed."
            case (Failed(_, _), Passed(_, _)) => s"Right predicate of $expr passed."
            case (Failed(_, _), Failed(_, _)) => s"Both predicates of $expr failed. " +
              s"Left: ${sa.showResult(ra)} Right: ${sb.showResult(rb)}"
          }
        }
      }
  }

  object AllOf {

    implicit def allOfHNilValidator[T]: Validator.Flat[T, AllOf[HNil]] =
      Validator.constant(t => Passed(t, AllOf(HList())))

    implicit def allOfHConsValidator[T, PH, PT <: HList, RH, RT <: HList](
      implicit
      vh: Validator[T, PH, RH], vt: Validator[T, AllOf[PT], AllOf[RT]]
    ): Validator[T, AllOf[PH :: PT], AllOf[vh.Res :: RT]] =
      Validator.instance(t => {
        val rh = vh.validate(t)
        val rt = vt.validate(t)
        Result.fromBoolean(rh.isPassed && rt.isPassed, t, AllOf(rh :: rt.predicate.ps))
      }, vh.isConstant && vt.isConstant)

    implicit def allOfHNilShow[T]: Show.Flat[T, AllOf[HNil]] =
      Show.instance(t => "true")

    implicit def allOfHConsShow[T, PH, PT <: HList, RH, RT <: HList](
      implicit
      sh: Show[T, PH, RH], st: Show[T, AllOf[PT], AllOf[RT]]
    ): Show[T, AllOf[PH :: PT], AllOf[sh.Res :: RT]] =
      new Show[T, AllOf[PH :: PT], AllOf[sh.Res :: RT]] {
        override def showExpr(t: T): String =
          accumulateShowExpr(t).mkString("(", " && ", ")")

        override def accumulateShowExpr(t: T): List[String] =
          sh.showExpr(t) :: st.accumulateShowExpr(t)
      }
  }

  object AnyOf {

    implicit def anyOfHNilValidator[T]: Validator.Flat[T, AnyOf[HNil]] =
      Validator.constant(t => Failed(t, AnyOf(HList())))

    implicit def anyOfHConsValidator[T, PH, PT <: HList, RH, RT <: HList](
      implicit
      vh: Validator[T, PH, RH], vt: Validator[T, AnyOf[PT], AnyOf[RT]]
    ): Validator[T, AnyOf[PH :: PT], AnyOf[vh.Res :: RT]] =
      Validator.instance(t => {
        val rh = vh.validate(t)
        val rt = vt.validate(t)
        Result.fromBoolean(rh.isPassed || rt.isPassed, t, AnyOf(rh :: rt.predicate.ps))
      }, vh.isConstant && vt.isConstant)

    implicit def anyOfHNilShow[T]: Show.Flat[T, AnyOf[HNil]] =
      Show.instance(t => "false")

    implicit def anyOfHConsShow[T, PH, PT <: HList, RH, RT <: HList](
      implicit
      sh: Show[T, PH, RH], st: Show[T, AnyOf[PT], AnyOf[RT]]
    ): Show[T, AnyOf[PH :: PT], AnyOf[sh.Res :: RT]] =
      new Show[T, AnyOf[PH :: PT], AnyOf[sh.Res :: RT]] {
        override def showExpr(t: T): String =
          accumulateShowExpr(t).mkString("(", " || ", ")")

        override def accumulateShowExpr(t: T): List[String] =
          sh.showExpr(t) :: st.accumulateShowExpr(t)
      }
  }

  object OneOf {

    implicit def oneOfHNilValidator[T]: Validator.Flat[T, OneOf[HNil]] =
      Validator.constant(t => Failed(t, OneOf(HList())))

    implicit def oneOfHConsValidator[T, PH, PT <: HList, RH, RT <: HList](
      implicit
      vh: Validator[T, PH, RH], vt: Validator[T, OneOf[PT], OneOf[RT]], toList: ToList[RT, Result[_, _]]
    ): Validator[T, OneOf[PH :: PT], OneOf[vh.Res :: RT]] =
      Validator.instance(t => {
        val rh = vh.validate(t)
        val rt = vt.validate(t).predicate.ps
        val passed = (rh :: toList(rt)).count(_.isPassed) == 1
        Result.fromBoolean(passed, t, OneOf(rh :: rt))
      }, vh.isConstant && vt.isConstant)

    implicit def oneOfHNilShow[T]: Show.Flat[T, OneOf[HNil]] =
      Show.instance(t => "false")

    implicit def oneOfHConsShow[T, PH, PT <: HList, RH, RT <: HList](
      implicit
      sh: Show[T, PH, RH], st: Show[T, OneOf[PT], OneOf[RT]]
    ): Show[T, OneOf[PH :: PT], OneOf[sh.Res :: RT]] =
      new Show[T, OneOf[PH :: PT], OneOf[sh.Res :: RT]] {
        override def showExpr(t: T): String =
          accumulateShowExpr(t).mkString("oneOf(", ", ", ")")

        override def accumulateShowExpr(t: T): List[String] =
          sh.showExpr(t) :: st.accumulateShowExpr(t)
      }
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
