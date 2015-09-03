package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.char._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class CharValidatorSpec extends Properties("CharValidator") {

  property("Digit.isValid") = forAll { (c: Char) =>
    isValid[Digit](c) ?= c.isDigit
  }

  property("Letter.isValid") = forAll { (c: Char) =>
    isValid[Letter](c) ?= c.isLetter
  }

  property("LowerCase.isValid") = forAll { (c: Char) =>
    isValid[LowerCase](c) ?= c.isLower
  }

  property("UpperCase.isValid") = forAll { (c: Char) =>
    isValid[UpperCase](c) ?= c.isUpper
  }

  property("Whitespace.isValid") = forAll { (c: Char) =>
    isValid[Whitespace](c) ?= c.isWhitespace
  }
}
