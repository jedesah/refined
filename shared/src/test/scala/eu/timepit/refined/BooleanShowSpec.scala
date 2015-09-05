package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.{::, HNil}

class BooleanShowSpec extends Properties("BooleanShow") {

  property("True.showExpr") = secure {
    showExpr[True](()) ?= "true"
  }

  property("False.showExpr") = secure {
    showExpr[False](()) ?= "false"
  }

  property("Not.showExpr") = secure {
    showExpr[Not[True]](()) ?= "!true"
  }

  property("Not.showResult.F") = secure {
    showResult[Not[False]](()) ?= "Predicate false did not pass."
  }

  property("Not.showResult.T") = secure {
    showResult[Not[True]](()) ?= "Predicate true did not fail."
  }

  property("And.showExpr") = secure {
    showExpr[And[True, False]](()) ?= "(true && false)"
  }

  property("And.showResult.TT") = secure {
    showResult[And[True, True]](()) ?= "Both predicates of (true && true) passed."
  }

  property("And.showResult.TF") = secure {
    showResult[And[True, False]](()) ?=
      "Right predicate of (true && false) failed: Predicate failed: false."
  }

  property("And.showResult.FT") = secure {
    showResult[And[False, True]](()) ?=
      "Left predicate of (false && true) failed: Predicate failed: false."
  }

  property("And.showResult.FF") = secure {
    showResult[And[False, False]](()) ?=
      "Both predicates of (false && false) failed. Left: Predicate failed: false. Right: Predicate failed: false."
  }

  property("Or.showExpr") = secure {
    showExpr[Or[True, False]](()) ?= "(true || false)"
  }

  property("Or.showResult.TT") = secure {
    showResult[Or[True, True]](()) ?= "Both predicates of (true || true) passed."
  }

  property("Or.showResult.TF") = secure {
    showResult[Or[True, False]](()) ?= "Left predicate of (true || false) passed."
  }

  property("Or.showResult.FT") = secure {
    showResult[Or[False, True]](()) ?= "Right predicate of (false || true) passed."
  }

  property("Or.showResult.FF") = secure {
    showResult[Or[False, False]](()) ?=
      "Both predicates of (false || false) failed. Left: Predicate failed: false. Right: Predicate failed: false."
  }

  property("Xor.showExpr") = secure {
    showExpr[Xor[True, False]](()) ?= "(true ^ false)"
  }

  property("Xor.showResult.TT") = secure {
    showResult[Xor[True, True]](()) ?= "Both predicates of (true ^ true) passed."
  }

  property("Xor.showResult.TF") = secure {
    showResult[Xor[True, False]](()) ?= "Left predicate of (true ^ false) passed."
  }

  property("Xor.showResult.FT") = secure {
    showResult[Xor[False, True]](()) ?= "Right predicate of (false ^ true) passed."
  }

  property("Xor.showResult.FF") = secure {
    showResult[Xor[False, False]](()) ?=
      "Both predicates of (false ^ false) failed. Left: Predicate failed: false. Right: Predicate failed: false."
  }

  property("AllOf.showExpr") = secure {
    showExpr[AllOf[Digit :: Letter :: Whitespace :: HNil]]('0') ?=
      "(isDigit('0') && isLetter('0') && isWhitespace('0') && true)"
  }

  property("AnyOf.showExpr") = secure {
    showExpr[AnyOf[Digit :: LowerCase :: Whitespace :: HNil]]('c') ?=
      "(isDigit('c') || isLower('c') || isWhitespace('c') || false)"
  }

  property("OneOf.showExpr") = secure {
    showExpr[OneOf[Digit :: LowerCase :: UpperCase :: HNil]]('c') ?=
      "oneOf(isDigit('c'), isLower('c'), isUpper('c'), false)"
  }
}
