package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class NumericShowSpec extends Properties("NumericShow") {

  property("Less.showExpr") = secure {
    showExpr[Less[W.`1.1`.T]](0.1) ?= "(0.1 < 1.1)"
  }

  property("Greater.showExpr") = secure {
    showExpr[Greater[W.`1.1`.T]](0.1) ?= "(0.1 > 1.1)"
  }

  property("Less.Nat.showExpr") = secure {
    showExpr[Less[_5]](0) ?= "(0 < 5)"
  }

  property("LessEqual.Nat.showExpr") = secure {
    showExpr[LessEqual[_5]](0) ?= "!(0 > 5)"
  }

  property("Greater.Nat.showExpr") = secure {
    showExpr[Greater[_5]](0) ?= "(0 > 5)"
  }

  property("GreaterEqual.Nat.showExpr") = secure {
    showExpr[GreaterEqual[_5]](0) ?= "!(0 < 5)"
  }
}
