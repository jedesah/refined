package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class NumericValidatorSpec extends Properties("NumericValidator") {

  property("Less.isValid") = forAll { (d: Double) =>
    isValid[Less[W.`1.0`.T]](d) ?= (d < 1.0)
  }

  property("Greater.isValid") = forAll { (d: Double) =>
    isValid[Greater[W.`1.0`.T]](d) ?= (d > 1.0)
  }

  property("Less.Nat ~= Less") = forAll { (i: Int) =>
    isValid[Less[_5]](i) ?= isValid[Less[W.`5`.T]](i)
  }

  property("Greater.Nat ~= Greater") = forAll { (i: Int) =>
    isValid[Greater[_5]](i) ?= isValid[Greater[W.`5`.T]](i)
  }

  property("Less.Nat.isValid") = forAll { (i: Int) =>
    isValid[Less[_5]](i) ?= (i < 5)
  }

  property("LessEqual.Nat.isValid") = forAll { (i: Int) =>
    isValid[LessEqual[_5]](i) ?= (i <= 5)
  }

  property("Greater.Nat.isValid") = forAll { (i: Int) =>
    isValid[Greater[_5]](i) ?= (i > 5)
  }

  property("GreaterEqual.Nat.isValid") = forAll { (i: Int) =>
    isValid[GreaterEqual[_5]](i) ?= (i >= 5)
  }

  property("Interval[_0, _1].isValid") = forAll { (d: Double) =>
    isValid[Interval[_0, _1]](d) ?= (d >= 0.0 && d <= 1.0)
  }

  property("Equal.Nat.Int.isValid") = forAll { (i: Int) =>
    isValid[Equal[_0]](i) ?= (i == 0)
  }

  property("Equal.Nat.Double.isValid") = forAll { (d: Double) =>
    isValid[Equal[_0]](d) ?= (d == 0.0)
  }

  property("Equal.Nat ~= Equal") = forAll { (i: Int) =>
    isValid[Equal[_1]](i) ?= isValid[Equal[W.`1`.T]](i)
  }

  property("Positive != NonPositive") = forAll { (i: Int) =>
    validate[Positive](i).isPassed == validate[NonPositive](i).isFailed
  }
}
