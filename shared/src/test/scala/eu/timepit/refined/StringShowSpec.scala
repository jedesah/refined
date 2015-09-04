package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.string._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class StringShowSpec extends Properties("StringShow") {

  property("EndsWith[S].showExpr") = secure {
    showExpr[EndsWith[W.`"cd"`.T]]("abcd") ?= """"abcd".endsWith("cd")"""
  }

  property("MatchesRegex[R].showExpr") = secure {
    showExpr[MatchesRegex[W.`".{2,10}"`.T]]("Hello") ?= """"Hello".matches(".{2,10}")"""
  }

  property("Regex.showExpr") = secure {
    showExpr[Regex]("(a|b)") ?= """isValidRegex("(a|b)")"""
  }

  property("StartsWith[S].showExpr") = secure {
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
}
