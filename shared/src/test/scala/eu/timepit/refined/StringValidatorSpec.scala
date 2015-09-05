package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class StringValidatorSpec extends Properties("StringValidator") {

  property("Count[LowerCase, Greater[_2]].isValid") = forAll { (s: String) =>
    isValid[Count[LowerCase, Greater[_2]]](s) ?= (s.count(_.isLower) > 2)
  }

  property("Empty.isValid") = forAll { (s: String) =>
    isValid[Empty](s) ?= s.isEmpty
  }

  property("NonEmpty.isValid") = forAll { (s: String) =>
    isValid[NonEmpty](s) ?= s.nonEmpty
  }

  property("Forall[LowerCase].isValid") = forAll { (s: String) =>
    isValid[Forall[LowerCase]](s) ?= s.forall(_.isLower)
  }

  property("Head[Digit].isValid") = forAll { (s: String) =>
    isValid[Head[Digit]](s) ?= s.headOption.fold(false)(_.isDigit)
  }

  property("Last[Letter].isValid") = forAll { (s: String) =>
    isValid[Last[Letter]](s) ?= s.lastOption.fold(false)(_.isLetter)
  }

  property("Size.isValid") = forAll { (s: String) =>
    isValid[Size[LessEqual[_10]]](s) ?= (s.length <= 10)
  }

  property("MinSize[_5].isValid") = forAll { (s: String) =>
    isValid[MinSize[_5]](s) ?= (s.length >= 5)
  }

  property("MatchesRegex[R].isValid") = forAll { (s: String) =>
    isValid[MatchesRegex[W.`".{2,10}"`.T]](s) ?= s.matches(".{2,10}")
  }

  property("EndsWith[S].isValid") = secure {
    val s = "abcd"
    isValid[EndsWith[W.`"cd"`.T]](s) ?= s.endsWith("cd")
  }

  property("StartsWith[S].isValid") = secure {
    val s = "abcd"
    isValid[StartsWith[W.`"ab"`.T]](s) ?= s.startsWith("ab")
  }

  property("Regex.isValid") = secure {
    isValid[Regex](".*")
  }

  property("Regex.notValid") = secure {
    !isValid[Regex]("(a|b")
  }

  property("Uri.isValid") = secure {
    isValid[Uri]("/a/b/c")
  }

  property("Uri.notValid") = secure {
    !isValid[Uri](" /a/b/c")
  }

  property("Uuid.isValid") = secure {
    isValid[Uuid]("9ecce884-47fe-4ba4-a1bb-1a3d71ed6530")
  }

  property("Uuid.notValid") = secure {
    !isValid[Uuid]("whops")
  }
}
