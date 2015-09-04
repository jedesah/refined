package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.char.{Digit, LowerCase}
import eu.timepit.refined.collection._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class CollectionValidatorSpec extends Properties("CollectionValidator") {

  property("Contains[W.`1`.T].isValid") = forAll { (l: List[Int]) =>
    isValid[Contains[W.`1`.T]](l) ?= l.contains(1)
  }

  property("Count[LowerCase, Greater[_2]].isValid") = forAll { (l: List[Char]) =>
    isValid[Count[LowerCase, Greater[_2]]](l) ?= (l.count(_.isLower) > 2)
  }

  property("Empty.isValid") = forAll { (l: List[Int]) =>
    isValid[Empty](l) ?= l.isEmpty
  }

  property("Exists[Equal[_]].isValid") = forAll { (l: List[Int]) =>
    isValid[Exists[Equal[_1]]](l) ?= l.contains(1)
  }

  property("Index[W.`2`.T, Digit].isValid") = forAll { (l: List[Char]) =>
    isValid[Index[W.`2`.T, Digit]](l) ?= l.lift(2).fold(false)(_.isDigit)
  }

  property("Last[Greater[_5]].isValid") = forAll { (l: List[Int]) =>
    isValid[Last[Greater[_5]]](l) ?= l.lastOption.fold(false)(_ > 5)
  }

  property("Size[Greater[_]].isValid") = forAll { (l: List[Int]) =>
    isValid[Size[Greater[_5]]](l) ?= (l.size > 5)
  }
}
