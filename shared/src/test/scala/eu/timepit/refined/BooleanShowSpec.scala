package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean._
import org.scalacheck.Prop._
import org.scalacheck.Properties

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

  property("Not.showResult") = secure {
    showResult[Not[True]](()) ?= "Predicate !true did not pass."
  }

  property("And.showExpr") = secure {
    showExpr[And[True, False]](()) ?= "(true && false)"
  }

  property("Or.showExpr") = secure {
    showExpr[Or[True, False]](()) ?= "(true || false)"
  }
}
