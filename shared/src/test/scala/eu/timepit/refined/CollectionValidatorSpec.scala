package eu.timepit.refined

import eu.timepit.refined.char.{Digit, LowerCase}
import eu.timepit.refined.collection._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class CollectionValidatorSpec extends Properties("CollectionValidator") {

  property("Contains[W.`0`.T].isValid") = forAll { (l: List[Int]) =>
    Validator[List[Int], Contains[W.`0`.T]].get.isValid(l) ?= l.contains(0)
  }

  property("Count[LowerCase, Greater[_2]].isValid") = forAll { (l: List[Char]) =>
    Validator[List[Char], Count[LowerCase, Greater[_2]]].get.isValid(l) ?= (l.count(_.isLower) > 2)
  }

  property("Empty.isValid") = forAll { (l: List[Int]) =>
    Validator[List[Int], Empty].get.isValid(l) ?= l.isEmpty
  }

  property("Exists[Equal[_]].isValid") = forAll { (l: List[Int]) =>
    Validator[List[Int], Exists[Equal[_1]]].get.isValid(l) ?= l.contains(1)
  }

  property("Index[W.`2`.T, Digit].isValid") = forAll { (l: List[Char]) =>
    Validator[List[Char], Index[W.`2`.T, Digit]].get.isValid(l) ?=
      l.lift(2).fold(false)(_.isDigit)
  }

  property("Last[Greater[_5]].isValid") = forAll { (l: List[Int]) =>
    Validator[List[Int], Last[Greater[_5]]].get.isValid(l) ?= l.lastOption.fold(false)(_ > 5)
  }

  property("Size[Greater[_]].isValid") = forAll { (l: List[Int]) =>
    Validator[List[Int], Size[Greater[_5]]].get.isValid(l) ?= (l.size > 5)
  }
}
