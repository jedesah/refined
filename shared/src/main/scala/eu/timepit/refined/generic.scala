package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.Result.Passed
import eu.timepit.refined.generic._
import shapeless.ops.coproduct.ToHList
import shapeless.ops.hlist.ToList
import shapeless.ops.record.Keys
import shapeless.{Coproduct, HList, LabelledGeneric, Witness}

object generic extends GenericValidators with GenericShowInstances with GenericInferenceRules {

  /** Predicate that checks if a value is equal to `U`. */
  case class Equal[U](u: U)

  /** Predicate that checks if the constructor names of a sum type satisfy `P`. */
  case class ConstructorNames[P](p: P)

  /** Predicate that checks if the field names of a product type satisfy `P`. */
  case class FieldNames[P](p: P)

  /** Predicate that witnesses that the type of a value is a subtype of `U`. */
  case class Subtype[U]()

  /** Predicate that witnesses that the type of a value is a supertype of `U`. */
  case class Supertype[U]()
}

private[refined] trait GenericValidators {

  implicit def equalValidator[T, U <: T](implicit wu: Witness.Aux[U]): Validator.Flat[T, Equal[U]] =
    Validator.fromPredicate(_ == wu.value, Equal(wu.value))

  implicit def ctorNamesValidator[T, P, R, R0 <: Coproduct, R1 <: HList, K <: HList](
    implicit
    lg: LabelledGeneric.Aux[T, R0],
    cthl: ToHList.Aux[R0, R1],
    keys: Keys.Aux[R1, K],
    ktl: ToList[K, Symbol],
    v: Validator[List[String], P, R]
  ): Validator[T, ConstructorNames[P], ConstructorNames[v.Res]] =
    new Validator[T, ConstructorNames[P], ConstructorNames[v.Res]] {
      override def validate(t: T): Res = {
        val ctorNames = keys().toList.map(_.name)
        val rv = v.validate(ctorNames)
        rv.mapBoth(_ => t, _ => ConstructorNames(rv))
      }

      override val isConstant: Boolean = true
    }

  implicit def fieldNamesValidator[T, P, R, R0 <: HList, K <: HList](
    implicit
    lg: LabelledGeneric.Aux[T, R0],
    keys: Keys.Aux[R0, K],
    ktl: ToList[K, Symbol],
    v: Validator[List[String], P, R]
  ): Validator[T, FieldNames[P], FieldNames[v.Res]] =
    new Validator[T, FieldNames[P], FieldNames[v.Res]] {
      override def validate(t: T): Res = {
        val fieldNames = keys().toList.map(_.name)
        val rv = v.validate(fieldNames)
        rv.mapBoth(_ => t, _ => FieldNames(rv))
      }

      override val isConstant: Boolean = true
    }

  implicit def subtypeValidator[T, U >: T]: Validator.Flat[T, Subtype[U]] =
    Validator.constant(t => Passed(t, Subtype()))

  implicit def supertypeValidator[T, U <: T]: Validator.Flat[T, Supertype[U]] =
    Validator.constant(t => Passed(t, Supertype()))
}

private[refined] trait GenericShowInstances {

  implicit def equalShow[T, U <: T](implicit wu: Witness.Aux[U]): Show.Flat[T, Equal[U]] =
    Show.instance(t => s"($t == ${wu.value})")
}

private[refined] trait GenericInferenceRules {

  implicit def equalValidatorInference[T, U <: T, P, R](implicit v: Validator[T, P, R], wu: Witness.Aux[U]): Equal[U] ==> P =
    InferenceRule(v.isValid(wu.value), s"equalValidatorInference(${wu.value})")
}
