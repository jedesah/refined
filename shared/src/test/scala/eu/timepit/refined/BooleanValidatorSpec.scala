package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.numeric.{Greater, Less}
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._
import shapeless.{::, HNil}

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

  property("AllOf[Greater[_0] :: Less[_10] :: HNil].isValid") = forAll { (i: Int) =>
    isValid[AllOf[Greater[_0] :: Less[_10] :: HNil]](i) ?= (i > 0 && i < 10)
  }

  property("AnyOf[Digit :: LowerCase :: Whitespace :: HNil].isValid") = forAll { (c: Char) =>
    isValid[AnyOf[Digit :: LowerCase :: Whitespace :: HNil]](c) ?=
      (c.isDigit || c.isLower || c.isWhitespace)
  }

  property("OneOf[Digit :: LowerCase :: UpperCase :: HNil].isValid") = forAll { (c: Char) =>
    isValid[OneOf[Digit :: LowerCase :: UpperCase :: HNil]](c) ?=
      (List(c.isDigit, c.isLower, c.isUpper).count(identity) == 1)
  }
}
