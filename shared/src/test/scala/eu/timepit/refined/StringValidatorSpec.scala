package eu.timepit.refined

import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class StringValidatorSpec extends Properties("StringValidator") {

  property("Empty.isValid") = forAll { (s: String) =>
    Validator[String, Empty].get.isValid(s) ?= s.isEmpty
  }

  property("NonEmpty.isValid") = forAll { (s: String) =>
    Validator[String, NonEmpty].get.isValid(s) ?= s.nonEmpty
  }

  property("Forall[LowerCase].isValid") = forAll { (s: String) =>
    Validator[String, Forall[LowerCase]].get.isValid(s) ?= s.forall(_.isLower)
  }

  property("Head[Digit].isValid") = forAll { (s: String) =>
    Validator[String, Head[Digit]].get.isValid(s) ?= s.headOption.fold(false)(_.isDigit)
  }

  property("Size.isValid") = forAll { (s: String) =>
    Validator[String, Size[LessEqual[_10]]].get.isValid(s) ?= (s.length <= 10)
  }

  property("Count[LowerCase, Greater[_2]].isValid") = forAll { (s: String) =>
    Validator[String, Count[LowerCase, Greater[_2]]].get.isValid(s) ?= (s.count(_.isLower) > 2)
  }

  property("MinSize[_5].isValid") = forAll { (s: String) =>
    Validator[String, MinSize[_5]].get.isValid(s) ?= (s.length >= 5)
  }

  property("MatchesRegex[R].isValid") = forAll { (s: String) =>
    Validator[String, MatchesRegex[W.`".{2,10}"`.T]].get.isValid(s) ?= s.matches(".{2,10}")
  }

  property("EndsWith[S].isValid") = secure {
    val s = "abcd"
    Validator[String, EndsWith[W.`"cd"`.T]].get.isValid(s) ?= s.endsWith("cd")
  }

  property("StartsWith[S].isValid") = secure {
    val s = "abcd"
    Validator[String, StartsWith[W.`"ab"`.T]].get.isValid(s) ?= s.startsWith("ab")
  }

  property("Regex.isValid") = secure {
    Validator[String, Regex].get.isValid(".*")
  }

  property("Regex.notValid") = secure {
    Validator[String, Regex].get.notValid("(a|b")
  }

  property("Uri.isValid") = secure {
    Validator[String, Uri].get.isValid("/a/b/c")
  }

  property("Uri.notValid") = secure {
    Validator[String, Uri].get.notValid(" /a/b/c")
  }

  property("Uuid.isValid") = secure {
    Validator[String, Uuid].get.isValid("9ecce884-47fe-4ba4-a1bb-1a3d71ed6530")
  }

  property("Uuid.notValid") = secure {
    Validator[String, Uuid].get.notValid("whops")
  }
}
