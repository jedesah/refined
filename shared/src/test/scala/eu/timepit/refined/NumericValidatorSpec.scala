package eu.timepit.refined

import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class NumericValidatorSpec extends Properties("NumericValidator") {

  property("Less.isValid") = forAll { (d: Double) =>
    Validator[Double, Less[W.`1.0`.T]].get.isValid(d) ?= (d < 1.0)
  }

  property("Greater.isValid") = forAll { (d: Double) =>
    Validator[Double, Greater[W.`1.0`.T]].get.isValid(d) ?= (d > 1.0)
  }

  property("Less.Nat ~= Less") = forAll { (i: Int) =>
    Validator[Int, Less[_5]].get.isValid(i) ?= Validator[Int, Less[W.`5`.T]].get.isValid(i)
  }

  property("Greater.Nat ~= Greater") = forAll { (i: Int) =>
    Validator[Int, Greater[_5]].get.isValid(i) ?= Validator[Int, Greater[W.`5`.T]].get.isValid(i)
  }

  property("Less.Nat.isValid") = forAll { (i: Int) =>
    Validator[Int, Less[_5]].get.isValid(i) ?= (i < 5)
  }

  property("LessEqual.Nat.isValid") = forAll { (i: Int) =>
    Validator[Int, LessEqual[_5]].get.isValid(i) ?= (i <= 5)
  }

  property("Greater.Nat.isValid") = forAll { (i: Int) =>
    Validator[Int, Greater[_5]].get.isValid(i) ?= (i > 5)
  }

  property("GreaterEqual.Nat.isValid") = forAll { (i: Int) =>
    Validator[Int, GreaterEqual[_5]].get.isValid(i) ?= (i >= 5)
  }

  property("Interval[_0, _1].isValid") = forAll { (d: Double) =>
    Validator[Double, Interval[_0, _1]].get.isValid(d) ?= (d >= 0.0 && d <= 1.0)
  }

  property("Equal.Nat.Int.isValid") = forAll { (i: Int) =>
    Validator[Int, Equal[_0]].get.isValid(i) ?= (i == 0)
  }

  property("Equal.Nat.Double.isValid") = forAll { (d: Double) =>
    Validator[Double, Equal[_0]].get.isValid(d) ?= (d == 0.0)
  }

  property("Equal.Nat ~= Equal") = forAll { (i: Int) =>
    Validator[Int, Equal[_1]].get.isValid(i) ?= Validator[Int, Equal[W.`1`.T]].get.isValid(i)
  }
}
