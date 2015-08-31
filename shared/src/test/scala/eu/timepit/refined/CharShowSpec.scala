package eu.timepit.refined

import eu.timepit.refined.char._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class CharShowSpec extends Properties("CharShow") {

  property("Digit.show") = secure {
    Show[Char, Digit].get.show('c') ?= "isDigit('c')"
  }

  property("Letter.show") = secure {
    Show[Char, Letter].get.show('c') ?= "isLetter('c')"
  }

  property("LowerCase.show") = secure {
    Show[Char, LowerCase].get.show('c') ?= "isLower('c')"
  }

  property("UpperCase.show") = secure {
    Show[Char, UpperCase].get.show('c') ?= "isUpper('c')"
  }

  property("Whitespace.show") = secure {
    Show[Char, Whitespace].get.show('c') ?= "isWhitespace('c')"
  }
}
