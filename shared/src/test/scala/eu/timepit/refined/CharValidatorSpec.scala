package eu.timepit.refined

import eu.timepit.refined.char._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class CharValidatorSpec extends Properties("CharValidator") {

  property("Digit.isValid") = forAll { (c: Char) =>
    Validator[Char, Digit].get.isValid(c) ?= c.isDigit
  }

  property("LowerCase.isValid") = forAll { (c: Char) =>
    Validator[Char, LowerCase].get.isValid(c) ?= c.isLower
  }

  property("UpperCase.isValid") = forAll { (c: Char) =>
    Validator[Char, UpperCase].get.isValid(c) ?= c.isUpper
  }
}
