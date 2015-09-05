package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.char._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class CharShowSpec extends Properties("CharShow") {

  property("Digit.showExpr") = secure {
    showExpr[Digit]('c') ?= "isDigit('c')"
  }

  property("Digit.showResult.Failed") = secure {
    showResult[Digit]('c') ?= "Predicate failed: isDigit('c')."
  }

  property("Letter.showExpr") = secure {
    showExpr[Letter]('c') ?= "isLetter('c')"
  }

  property("Letter.showResult.Passed") = secure {
    showResult[Letter]('c') ?= "Predicate passed: isLetter('c')."
  }

  property("LowerCase.showExpr") = secure {
    showExpr[LowerCase]('c') ?= "isLower('c')"
  }

  property("UpperCase.showExpr") = secure {
    showExpr[UpperCase]('c') ?= "isUpper('c')"
  }

  property("Whitespace.showExpr") = secure {
    showExpr[Whitespace]('c') ?= "isWhitespace('c')"
  }
}
