package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.api.Validate
import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.numeric.{ Greater, Less }
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._
import shapeless.{ ::, HNil }

class BooleanValidateSpec extends Properties("BooleanValidate") {

  type FF[Op[_, _]] = False Op False
  type FT[Op[_, _]] = False Op True
  type TF[Op[_, _]] = True Op False
  type TT[Op[_, _]] = True Op True

  property("True.isValid") = secure {
    isValid[True](())
  }

  property("True.showExpr") = secure {
    showExpr[True](()) ?= "true"
  }

  property("False.isValid") = secure {
    notValid[False](())
  }

  property("False.showExpr") = secure {
    showExpr[False](()) ?= "false"
  }

  property("Not.isValid") = secure {
    isValid[Not[False]](())
  }

  property("Not.showExpr") = secure {
    showExpr[Not[True]](()) ?= "!true"
  }

  property("Not.showResult") = secure {
    showResult[Not[False]](()) ?= "Predicate false did not pass."
  }

  property("And.isValid") = secure {
    notValid[FF[And]](()) &&
      notValid[FT[And]](()) &&
      notValid[TF[And]](()) &&
      isValid[TT[And]](())
  }

  property("And.showExpr") = secure {
    showExpr[TF[And]](()) ?= "(true && false)"
  }

  property("And.showResult") = secure {
    (showResult[TT[And]](()) ?= "Both predicates of (true && true) passed.") &&
      (showResult[FT[And]](()) ?= "Left predicate of (false && true) failed: Predicate failed: false.") &&
      (showResult[TF[And]](()) ?= "Right predicate of (true && false) failed: Predicate failed: false.") &&
      (showResult[FF[And]](()) ?= "Both predicates of (false && false) failed. " +
        "Left: Predicate failed: false. Right: Predicate failed: false.")
  }

  property("Or.isValid") = secure {
    notValid[FF[Or]](()) &&
      isValid[FT[Or]](()) &&
      isValid[TF[Or]](()) &&
      isValid[TT[Or]](())
  }

  property("Or.showExpr") = secure {
    showExpr[TF[Or]](()) ?= "(true || false)"
  }

  property("Or.showResult") = secure {
    (showResult[TT[Or]](()) ?= "Both predicates of (true || true) passed.") &&
      (showResult[FT[Or]](()) ?= "Right predicate of (false || true) passed.") &&
      (showResult[TF[Or]](()) ?= "Left predicate of (true || false) passed.") &&
      (showResult[FF[Or]](()) ?= "Both predicates of (false || false) failed. " +
        "Left: Predicate failed: false. Right: Predicate failed: false.")
  }

  property("Xor.isValid") = secure {
    notValid[FF[Xor]](()) &&
      isValid[FT[Xor]](()) &&
      isValid[TF[Xor]](()) &&
      notValid[TT[Xor]](())
  }

  property("Xor.showExpr") = secure {
    showExpr[TF[Xor]](()) ?= "(true ^ false)"
  }

  property("Xor.showResult") = secure {
    (showResult[TT[Xor]](()) ?= "Both predicates of (true ^ true) passed.") &&
      (showResult[FT[Xor]](()) ?= "Right predicate of (false ^ true) passed.") &&
      (showResult[TF[Xor]](()) ?= "Left predicate of (true ^ false) passed.") &&
      (showResult[FF[Xor]](()) ?= "Both predicates of (false ^ false) failed. " +
        "Left: Predicate failed: false. Right: Predicate failed: false.")
  }

  property("AllOf.isValid") = forAll { (i: Int) =>
    isValid[AllOf[Greater[_0] :: Less[_10] :: HNil]](i) ?= (i > 0 && i < 10)
  }

  property("AllOf.showExpr") = secure {
    showExpr[AllOf[Greater[_0] :: Less[_10] :: HNil]](5) ?=
      "((5 > 0) && (5 < 10) && true)"
  }

  property("AnyOf.isValid") = forAll { (c: Char) =>
    isValid[AnyOf[Digit :: LowerCase :: Whitespace :: HNil]](c) ?=
      (c.isDigit || c.isLower || c.isWhitespace)
  }

  property("AnyOf.showExpr") = secure {
    showExpr[AnyOf[Digit :: LowerCase :: Whitespace :: HNil]]('c') ?=
      "(isDigit('c') || isLower('c') || isWhitespace('c') || false)"
  }

  property("OneOf.isValid") = forAll { (c: Char) =>
    isValid[OneOf[Digit :: LowerCase :: UpperCase :: HNil]](c) ?=
      List(c.isDigit, c.isLower, c.isUpper).count(identity) == 1
  }

  property("OneOf.showExpr") = secure {
    showExpr[OneOf[Digit :: LowerCase :: UpperCase :: HNil]]('c') ?=
      "oneOf(isDigit('c'), isLower('c'), isUpper('c'), false)"
  }

  property("OneOf.contramap(identity).accumulateShow") = secure {
    val v = Validate[Char, OneOf[Digit :: LowerCase :: HNil]]
    v.contramap(identity[Char]).accumulateShowExpr('c') ?= v.accumulateShowExpr('c')
  }
}
