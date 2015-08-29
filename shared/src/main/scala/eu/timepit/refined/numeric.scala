package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.boolean._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import shapeless.nat._
import shapeless.ops.nat.ToInt
import shapeless.{Nat, Witness}

/**
 * Module for numeric predicates. Predicates that take type parameters
 * support both shapeless' natural numbers (`Nat`) and numeric singleton
 * types (which are made available by shapeless' `Witness` - abbreviated
 * as [[W]] in refined) which include subtypes of `Int`, `Long`, `Double`,
 * `Char` etc.
 *
 * Example: {{{
 * scala> import shapeless.nat._
 *      | import shapeless.tag.@@
 *      | import eu.timepit.refined.numeric._
 *
 * scala> refineMT[Greater[_5]](10)
 * res1: Int @@ Greater[_5] = 10
 *
 * scala> refineMT[Greater[W.`1.5`.T]](1.6)
 * res2: Double @@ Greater[W.`1.5`.T] = 1.6
 * }}}
 */
object numeric extends NumericValidators with NumericInferenceRules {

  /** Predicate that checks if a numeric value is less than `N`. */
  case class Less[N](n: N)

  /** Predicate that checks if a numeric value is greater than `N`. */
  case class Greater[N](n: N)

  /** Predicate that checks if a numeric value is less than or equal to `N`. */
  type LessEqual[N] = Not[Greater[N]]

  /** Predicate that checks if a numeric value is greater than or equal to `N`. */
  type GreaterEqual[N] = Not[Less[N]]

  /** Predicate that checks if a numeric value is positive (> 0). */
  type Positive = Greater[_0]

  /** Predicate that checks if a numeric value is zero or negative (<= 0). */
  type NonPositive = Not[Positive]

  /** Predicate that checks if a numeric value is negative (< 0). */
  type Negative = Less[_0]

  /** Predicate that checks if a numeric value is zero or positive (>= 0). */
  type NonNegative = Not[Negative]

  /** Predicate that checks if a numeric value is in the interval `[L, H]`. */
  type Interval[L, H] = GreaterEqual[L] And LessEqual[H]
}

private[refined] trait NumericValidators {

  implicit def lessValidator[T, N <: T](implicit wn: Witness.Aux[N], nt: Numeric[T]): Validator.Flat[T, Less[N]] =
    Validator.fromPredicate(t => nt.lt(t, wn.value), Less(wn.value))

  implicit def greaterValidator[T, N <: T](implicit wn: Witness.Aux[N], nt: Numeric[T]): Validator.Flat[T, Greater[N]] =
    Validator.fromPredicate(t => nt.gt(t, wn.value), Greater(wn.value))

  implicit def lessValidatorNat[N <: Nat, T](implicit tn: ToInt[N], wn: Witness.Aux[N], nt: Numeric[T]): Validator.Flat[T, Less[N]] =
    Validator.fromPredicate(t => nt.toDouble(t) < tn(), Less(wn.value))

  implicit def greaterValidatorNat[N <: Nat, T](implicit tn: ToInt[N], wn: Witness.Aux[N], nt: Numeric[T]): Validator.Flat[T, Greater[N]] =
    Validator.fromPredicate(t => nt.toDouble(t) > tn(), Greater(wn.value))

  implicit def equalValidatorNat[N <: Nat, T](implicit tn: ToInt[N], wn: Witness.Aux[N], nt: Numeric[T]): Validator.Flat[T, Equal[N]] =
    Validator.fromPredicate(t => nt.toDouble(t) == tn(), Equal(wn.value))
}

private[refined] trait NumericInferenceRules {

  implicit def lessInferenceWit[C, A <: C, B <: C](implicit wa: Witness.Aux[A], wb: Witness.Aux[B], nc: Numeric[C]): Less[A] ==> Less[B] =
    InferenceRule(nc.lt(wa.value, wb.value), s"lessInferenceWit(${wa.value}, ${wb.value})")

  implicit def greaterInferenceWit[C, A <: C, B <: C](implicit wa: Witness.Aux[A], wb: Witness.Aux[B], nc: Numeric[C]): Greater[A] ==> Greater[B] =
    InferenceRule(nc.gt(wa.value, wb.value), s"greaterInferenceWit(${wa.value}, ${wb.value})")

  implicit def lessInferenceNat[A <: Nat, B <: Nat](implicit ta: ToInt[A], tb: ToInt[B]): Less[A] ==> Less[B] =
    InferenceRule(ta() < tb(), s"lessInferenceNat(${ta()}, ${tb()})")

  implicit def greaterInferenceNat[A <: Nat, B <: Nat](implicit ta: ToInt[A], tb: ToInt[B]): Greater[A] ==> Greater[B] =
    InferenceRule(ta() > tb(), s"greaterInferenceNat(${ta()}, ${tb()})")

  implicit def lessInferenceWitNat[C, A <: C, B <: Nat](implicit wa: Witness.Aux[A], tb: ToInt[B], nc: Numeric[C]): Less[A] ==> Less[B] =
    InferenceRule(nc.lt(wa.value, nc.fromInt(tb())), s"lessInferenceWitNat(${wa.value}, ${tb()})")

  implicit def greaterInferenceWitNat[C, A <: C, B <: Nat](implicit wa: Witness.Aux[A], tb: ToInt[B], nc: Numeric[C]): Greater[A] ==> Greater[B] =
    InferenceRule(nc.gt(wa.value, nc.fromInt(tb())), s"greaterInferenceWitNat(${wa.value}, ${tb()})")

  implicit def equalValidatorInferenceNat[T, P, R, N <: Nat](implicit v: Validator[T, P, R], nt: Numeric[T], tn: ToInt[N]): Equal[N] ==> P =
    InferenceRule(v.isValid(nt.fromInt(tn())), s"equalValidatorInferenceNat(${nt.fromInt(tn())})")
}
