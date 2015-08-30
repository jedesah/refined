package eu.timepit.refined

import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class StringPredicateSpec extends Properties("StringPredicate") {

  /*

  property("Empty.show") = secure {
    Predicate[Empty, String].show("test") ?= "isEmpty(test)"
  }

  property("NonEmpty.show") = secure {
    Predicate[NonEmpty, String].show("test") ?= "!isEmpty(test)"
  }

  property("Forall[LowerCase].show") = secure {
    Predicate[Forall[LowerCase], String].show("abc") ?=
      "(isLower('a') && isLower('b') && isLower('c'))"
  }

  property("Forall[UpperCase].show") = secure {
    Predicate[Forall[UpperCase], String].show("abc") ?=
      "(isUpper('a') && isUpper('b') && isUpper('c'))"
  }

  property("Last[Digit].show") = secure {
    Predicate[Last[Digit], String].show("abc0") ?= "isDigit('0')"
  }

  property("Size.show") = secure {
    type P = Size[Greater[_5] And LessEqual[_10]]
    Predicate[P, String].show("test") ?= "((4 > 5) && !(4 > 10))"
  }

  property("MatchesRegex[R].show") = secure {
    Predicate[MatchesRegex[W.`".{2,10}"`.T], String].show("Hello") ?=
      """"Hello".matches(".{2,10}")"""
  }

  property("EndsWith[S].show") = secure {
    val s = "abcd"
    Predicate[EndsWith[W.`"cd"`.T], String].show(s) ?= """"abcd".endsWith("cd")"""
  }

  property("StartsWith[S].show") = secure {
    val s = "abcd"
    Predicate[StartsWith[W.`"ab"`.T], String].show(s) ?= """"abcd".startsWith("ab")"""
  }

  property("Regex.show") = secure {
    Predicate[Regex, String].show("(a|b)") ?= """isValidRegex("(a|b)")"""
  }

  property("Uri.validate failure") = secure {
    val jvmErr = Predicate[Uri, String].validate(" /a/b/c") ?=
      Some("Uri predicate failed: Illegal character in path at index 0:  /a/b/c")

    val jsErr = Predicate[Uri, String].validate(" /a/b/c") ?=
      Some("Uri predicate failed: Malformed URI in  /a/b/c at -1")

    jvmErr || jsErr
  }

  property("Uuid.validate failure") = secure {
    Predicate[Uuid, String].validate("whops") ?=
      Some("Uuid predicate failed: Invalid UUID string: whops")
  }
  */
}
