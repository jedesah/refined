package eu.timepit.refined

import eu.timepit.refined.char.{Digit, LowerCase}
import eu.timepit.refined.collection._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class CollectionPredicateSpec extends Properties("CollectionPredicate") {
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

  /*

property("Empty.show") = secure {
  Show[Empty, String].show("test") ?= "isEmpty(test)"
}

property("NonEmpty.show") = secure {
  Show[NonEmpty, String].show("test") ?= "!isEmpty(test)"
}

property("Forall[LowerCase].show") = secure {
  Show[Forall[LowerCase], String].show("abc") ?=
    "(isLower('a') && isLower('b') && isLower('c'))"
}

property("Forall[UpperCase].show") = secure {
  Show[Forall[UpperCase], String].show("abc") ?=
    "(isUpper('a') && isUpper('b') && isUpper('c'))"
}

property("Last[Digit].show") = secure {
  Show[Last[Digit], String].show("abc0") ?= "isDigit('0')"
}

property("Size.show") = secure {
  type P = Size[Greater[_5] And LessEqual[_10]]
  Show[P, String].show("test") ?= "((4 > 5) && !(4 > 10))"
}

*/
}
