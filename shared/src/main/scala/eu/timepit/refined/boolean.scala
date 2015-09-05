package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.Result.{Failed, Passed}
import eu.timepit.refined.boolean._
import shapeless.ops.hlist.ToList
import shapeless.{::, HList, HNil}

object boolean extends BooleanValidators with BooleanShowInstances with BooleanInferenceRules0 {

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
      new Validator[T, Not[P], Not[v.Res]] {
        override def validate(t: T): Res = {
          val r = v.validate(t)
          r match {
            case Passed(_, _) => Failed(t, Not(r))
            case Failed(_, _) => Passed(t, Not(r))
          }
        }

        override def isConstant: Boolean = v.isConstant
      }

    implicit def notShow[T, P, R](implicit s: Show[T, P, R]): Show[T, Not[P], Not[s.Res]] =
      new Show[T, Not[P], Not[s.Res]] {
        override def showExpr(t: T): String = s"!${s.showExpr(t)}"

        override def showResult(r: Res): String =
          r match {
            case Passed(t, _) => s"Predicate ${s.showExpr(t)} did not pass."
            case Failed(t, _) => s"Predicate ${s.showExpr(t)} did not fail."
          }
      }

    implicit def doubleNegationElimination[A, B](implicit p1: A ==> B): Not[Not[A]] ==> B =
      p1.adapt("doubleNegationElimination(%s)")

    implicit def doubleNegationIntroduction[A, B](implicit p1: A ==> B): A ==> Not[Not[B]] =
      p1.adapt("doubleNegationIntroduction(%s)")
  }
}

private[refined] trait BooleanValidators {

  implicit def andValidator[T, A, B, RA, RB](
    implicit
    va: Validator[T, A, RA], vb: Validator[T, B, RB]
  ): Validator[T, A And B, va.Res And vb.Res] =
    Validator.instance(t => {
      val p = And(va.validate(t), vb.validate(t))
      (p.a, p.b) match {
        case (Passed(_, _), Passed(_, _)) => Passed(t, p)
        case _ => Failed(t, p)
      }
    }, va.isConstant && vb.isConstant)

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

private[refined] trait BooleanShowInstances {

  implicit def andShow[T, A, B, RA, RB](
    implicit
    sa: Show[T, A, RA], sb: Show[T, B, RB]
  ): Show[T, A And B, sa.Res And sb.Res] =
    new Show[T, A And B, sa.Res And sb.Res] {
      override def showExpr(t: T): String = s"(${sa.showExpr(t)} && ${sb.showExpr(t)})"

      override def showResult(r: Res): String = {
        val expr = showExpr(r.value)
        (r.predicate.a, r.predicate.b) match {
          case (Passed(_, _), Passed(_, _)) =>
            s"Both predicates of $expr passed."

          case (ar @ Failed(_, _), br @ Failed(_, _)) =>
            s"Both predicates of $expr failed. Left: ${sa.showResult(ar)} Right: ${sb.showResult(br)}"

          case (ar @ Failed(_, _), _) =>
            s"Left predicate of $expr failed: ${sa.showResult(ar)}"

          case (_, br @ Failed(_, _)) =>
            s"Right predicate of $expr failed: ${sb.showResult(br)}"
        }
      }
    }

  implicit def orShow[T, A, B, RA, RB](
    implicit
    sa: Show[T, A, RA], sb: Show[T, B, RB]
  ): Show[T, A Or B, sa.Res Or sb.Res] =
    new Show[T, A Or B, sa.Res Or sb.Res] {
      override def showExpr(t: T): String = s"(${sa.showExpr(t)} || ${sb.showExpr(t)})"

      override def showResult(r: Res): String = {
        val expr = showExpr(r.value)
        (r.predicate.a, r.predicate.b) match {
          case (Passed(_, _), Passed(_, _)) =>
            s"Both predicates of $expr passed."

          case (Passed(_, _), _) =>
            s"Left predicate of $expr passed."

          case (_, Passed(_, _)) =>
            s"Right predicate of $expr passed."

          case (ar @ Failed(_, _), br @ Failed(_, _)) =>
            s"Both predicates of $expr failed. Left: ${sa.showResult(ar)} Right: ${sb.showResult(br)}"
        }
      }
    }

  implicit def xorShow[T, A, B, RA, RB](
    implicit
    sa: Show[T, A, RA], sb: Show[T, B, RB]
  ): Show[T, A Xor B, sa.Res Xor sb.Res] =
    new Show[T, A Xor B, sa.Res Xor sb.Res] {
      override def showExpr(t: T): String = s"(${sa.showExpr(t)} ^ ${sb.showExpr(t)})"

      override def showResult(r: Res): String = {
        val expr = showExpr(r.value)
        (r.predicate.a, r.predicate.b) match {
          case (Passed(_, _), Passed(_, _)) =>
            s"Both predicates of $expr passed."

          case (ar @ Failed(_, _), br @ Failed(_, _)) =>
            s"Both predicates of $expr failed: Left: ${sa.showResult(ar)} Right: ${sb.showResult(br)}"

          case (Passed(_, _), _) =>
            s"Left predicate of $expr passed."

          case (_, Passed(_, _)) =>
            s"Right predicate of $expr passed."
        }
      }
    }

  implicit def allOfHNilShow[T]: Show.Flat[T, AllOf[HNil]] =
    Show.instance(t => "true")

  implicit def allOfHConsShow[T, PH, PT <: HList, RH, RT <: HList](
    implicit
    sh: Show[T, PH, RH], st: Show[T, AllOf[PT], AllOf[RT]]
  ): Show[T, AllOf[PH :: PT], AllOf[sh.Res :: RT]] =
    new Show[T, AllOf[PH :: PT], AllOf[sh.Res :: RT]] {
      override def showExpr(t: T): String = s"(${sh.showExpr(t)} && ${st.showExpr(t)})"

      override def showResult(r: Res): String = super.showResult(r)
    }

  //Show.instance(t => s"(${sh.showExpr(t)} && ${st.showExpr(t)})")

  /*  implicit def allOfHNilValidator[T]: Validator.Flat[T, AllOf[HNil]] =
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
*/
}

private[refined] trait BooleanInferenceRules0 extends BooleanInferenceRules1 {

  implicit def minimalTautology[A]: A ==> A =
    InferenceRule.alwaysValid("minimalTautology")

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
