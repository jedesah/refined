package eu.timepit.refined

import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class NumericPredicateSpec extends Properties("NumericPredicate") {

  /*

  property("Less.show") = secure {
    Predicate[Less[W.`1.1`.T], Double].show(0.1) ?= "(0.1 < 1.1)"
  }

  property("Greater.show") = secure {
    Predicate[Greater[W.`1.1`.T], Double].show(0.1) ?= "(0.1 > 1.1)"
  }

  property("Less.Nat.show") = secure {
    Predicate[Less[_5], Int].show(0) ?= "(0 < 5)"
  }

  property("LessEqual.Nat.show") = secure {
    Predicate[LessEqual[_5], Int].show(0) ?= "!(0 > 5)"
  }

  property("Greater.Nat.show") = secure {
    Predicate[Greater[_5], Int].show(0) ?= "(0 > 5)"
  }

  property("GreaterEqual.Nat.show") = secure {
    Predicate[GreaterEqual[_5], Int].show(0) ?= "!(0 < 5)"
  }

  property("Equal.Nat.show") = secure {
    Predicate[Equal[_5], Int].show(0) ?= "(0 == 5)"
  }
  */
}
