package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean.And
import eu.timepit.refined.char.Digit
import eu.timepit.refined.collection.{Count, Empty, NonEmpty, Size}
import eu.timepit.refined.numeric.{Greater, LessEqual}
import eu.timepit.refined.string._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class StringShowSpec extends Properties("StringShow") {

  property("EndsWith.showExpr") = secure {
    showExpr[EndsWith[W.`"cd"`.T]]("abcd") ?= """"abcd".endsWith("cd")"""
  }

  property("MatchesRegex.showExpr") = secure {
    showExpr[MatchesRegex[W.`".{2,10}"`.T]]("Hello") ?= """"Hello".matches(".{2,10}")"""
  }

  property("Regex.showExpr") = secure {
    showExpr[Regex]("(a|b)") ?= """isValidRegex("(a|b)")"""
  }

  property("StartsWith.showExpr") = secure {
    showExpr[StartsWith[W.`"ab"`.T]]("abcd") ?= """"abcd".startsWith("ab")"""
  }

  property("Uri.showResult") = secure {
    val jvmErr = showResult[Uri](" /a/b/c") ?=
      "Uri predicate failed: Illegal character in path at index 0:  /a/b/c"

    val jsErr = showResult[Uri](" /a/b/c") ?=
      "Uri predicate failed: Malformed URI in  /a/b/c at -1"

    jvmErr || jsErr
  }

  property("Uuid.showResult") = secure {
    showResult[Uuid]("whops") ?= "Uuid predicate failed: Invalid UUID string: whops"
  }

  // collection predicates applied to String

  property("Count.showExpr") = secure {
    showExpr[Count[Digit, Greater[_2]]]("ab12") ?=
      "count(isDigit('a'), isDigit('b'), isDigit('1'), isDigit('2'))"
  }

  property("Empty.showExpr") = secure {
    showExpr[Empty]("test") ?= "isEmpty(test)"
  }

  property("NonEmpty.showExpr") = secure {
    showExpr[NonEmpty]("test") ?= "!isEmpty(test)"
  }

  /*
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
*/

  property("Size.showExpr") = secure {
    showExpr[Size[Greater[_5] And LessEqual[_10]]]("test") ?= "((4 > 5) && !(4 > 10))"
  }
}
