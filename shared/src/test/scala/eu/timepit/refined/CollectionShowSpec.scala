package eu.timepit.refined

import eu.timepit.refined.char.{Digit, LowerCase}
import eu.timepit.refined.collection._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class CollectionShowSpec extends Properties("CollectionShow") {
  /*

  property("Count[LowerCase, Greater[_2]].show") = secure {
    Predicate[Count[LowerCase, Greater[_2]], List[Char]].show(List('a', 'B')) ?= "(1 > 2)"
  }

  property("Count[LowerCase, Greater[_2]].validate") = secure {
    Predicate[Count[LowerCase, Greater[_2]], List[Char]].validate(List('a', 'B')) ?=
      Some("Predicate taking count(isLower('a'), isLower('B')) = 1 failed: Predicate failed: (1 > 2).")
  }

  property("Exists[Equal[_]].show") = secure {
    Predicate[Exists[Equal[_1]], List[Int]].show(List(1, 2, 3)) ?=
      "!(!(1 == 1) && !(2 == 1) && !(3 == 1))"
  }

  property("Index[W.`2`.T, Digit].validate") = secure {
    Predicate[Index[W.`2`.T, Digit], List[Char]].validate(List('a', 'b', 'c')) ?=
      Some("Predicate taking index(List(a, b, c), 2) = c failed: Predicate failed: isDigit('c').")
  }

  property("Index[W.`2`.T, Digit].validate") = secure {
    Predicate[Index[W.`2`.T, Digit], List[Char]].validate(List.empty) ?=
      Some("Predicate failed: empty collection.")
  }

  property("Last[Greater[_5]].show") = secure {
    Predicate[Last[Greater[_5]], List[Int]].show(List(1, 2, 3)) ?= "(3 > 5)"
  }

  property("Last[Greater[_5]].validate") = secure {
    Predicate[Last[Greater[_5]], List[Int]].validate(List(1, 2, 3)) ?=
      Some("Predicate taking last(List(1, 2, 3)) = 3 failed: Predicate failed: (3 > 5).")
  }
  */

}
