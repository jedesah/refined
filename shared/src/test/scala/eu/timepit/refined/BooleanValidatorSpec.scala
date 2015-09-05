package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class BooleanValidatorSpec extends Properties("BooleanValidator") {

  type FF[Op[_, _]] = False Op False
  type FT[Op[_, _]] = False Op True
  type TF[Op[_, _]] = True Op False
  type TT[Op[_, _]] = True Op True

  property("True.isValid") = secure {
    isValid[True](())
  }

  property("Not[True].isValid") = secure {
    !isValid[Not[True]](())
  }

  property("False.isValid") = secure {
    !isValid[False](())
  }

  property("Not[False].isValid") = secure {
    isValid[Not[False]](())
  }

  property("And.isValid") = secure {
    !isValid[FF[And]](()) && !isValid[FT[And]](()) && !isValid[TF[And]](()) && isValid[TT[And]](())
  }

  property("Or.isValid") = secure {
    !isValid[FF[Or]](()) && isValid[FT[Or]](()) && isValid[TF[Or]](()) && isValid[TT[Or]](())
  }

  property("Xor.isValid") = secure {
    !isValid[FF[Xor]](()) && isValid[FT[Xor]](()) && isValid[TF[Xor]](()) && !isValid[TT[Xor]](())
  }

  /*
  property("AllOf[Greater[_0] :: Less[_10] :: HNil].isValid") = forAll { (i: Int) =>
    Predicate[AllOf[Greater[_0] :: Less[_10] :: HNil], Int].isValid(i) ?=
      (i > 0 && i < 10)
  }

  property("AllOf[Greater[_0] :: Less[_10] :: HNil].show") = secure {
    Predicate[AllOf[Greater[_0] :: Less[_10] :: HNil], Int].show(5) ?=
      "((5 > 0) && ((5 < 10) && true))"
  }

  property("AnyOf[Digit :: LowerCase :: Whitespace :: HNil].isValid") = forAll { (c: Char) =>
    Predicate[AnyOf[Digit :: LowerCase :: Whitespace :: HNil], Char].isValid(c) ?=
      (c.isDigit || c.isLower || c.isWhitespace)
  }

  property("AnyOf[Digit :: LowerCase :: Whitespace :: HNil].show") = secure {
    Predicate[AnyOf[Digit :: LowerCase :: Whitespace :: HNil], Char].show('c') ?=
      "(isDigit('c') || (isLower('c') || (isWhitespace('c') || false)))"
  }

  property("OneOf[Digit :: LowerCase :: UpperCase :: HNil].isValid") = forAll { (c: Char) =>
    Predicate[OneOf[Digit :: LowerCase :: UpperCase :: HNil], Char].isValid(c) ?=
      List(c.isDigit, c.isLower, c.isUpper).count(identity) == 1
  }

  property("OneOf[Digit :: LowerCase :: UpperCase :: HNil].show") = secure {
    Predicate[OneOf[Digit :: LowerCase :: UpperCase :: HNil], Char].show('c') ?=
      "oneOf(isDigit('c'), isLower('c'), isUpper('c'), false)"
  }

  property("OneOf[_].contramap(identity).accumulateIsValid") = forAll { (c: Char) =>
    val p = Predicate[OneOf[Digit :: LowerCase :: UpperCase :: HNil], Char]
    p.contramap(identity[Char]).accumulateIsValid(c) ?= p.accumulateIsValid(c)
  }

  property("OneOf[_].contramap(identity).accumulateShow") = secure {
    val p = Predicate[OneOf[Digit :: LowerCase :: UpperCase :: HNil], Char]
    p.contramap(identity[Char]).accumulateShow('c') ?= p.accumulateShow('c')
  }
*/
}
