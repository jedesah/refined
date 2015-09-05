package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean._
import eu.timepit.refined.char.{Digit, Letter, Whitespace}
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

  property("Not.showResult.Passed") = secure {
    showResult[Not[False]](()) ?= "Predicate false did not pass."
  }

  property("Not.showResult.Failed") = secure {
    showResult[Not[True]](()) ?= "Predicate true did not fail."
  }

  property("And.showExpr") = secure {
    showExpr[And[True, False]](()) ?= "(true && false)"
  }

  property("And.showResult.Failed.R") = secure {
    showResult[And[True, False]](()) ?=
      "Right predicate of (true && false) failed: Predicate failed: false."
  }

  property("And.showResult.Failed.B") = secure {
    showResult[And[False, False]](()) ?=
      "Both predicates of (false && false) failed. Left: Predicate failed: false. Right: Predicate failed: false."
  }

  property("Or.showExpr") = secure {
    showExpr[Or[True, False]](()) ?= "(true || false)"
  }

  property("Xor.showExpr") = secure {
    showExpr[Xor[True, False]](()) ?= "(true ^ false)"
  }

  property("AllOf.showExpr") = secure {
    showExpr[AllOf[Digit :: Letter :: Whitespace :: HNil]]('0') ?=
      "(isDigit('0') && (isLetter('0') && (isWhitespace('0') && true)))"
  }

  /*

  property("AnyOf[Digit :: LowerCase :: Whitespace :: HNil].show") = secure {
    Predicate[AnyOf[Digit :: LowerCase :: Whitespace :: HNil], Char].show('c') ?=
      "(isDigit('c') || (isLower('c') || (isWhitespace('c') || false)))"
  }

  property("OneOf[Digit :: LowerCase :: UpperCase :: HNil].show") = secure {
    Predicate[OneOf[Digit :: LowerCase :: UpperCase :: HNil], Char].show('c') ?=
      "oneOf(isDigit('c'), isLower('c'), isUpper('c'), false)"
  }


  */
}
