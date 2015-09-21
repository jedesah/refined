package eu.timepit.refined

import eu.timepit.refined.api.{Show, Validate}
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.char._

object char extends CharInstances {

  /** Predicate that checks if a `Char` is a digit. */
  case class Digit()

  /** Predicate that checks if a `Char` is a letter. */
  case class Letter()

  /** Predicate that checks if a `Char` is a lower case character. */
  case class LowerCase()

  /** Predicate that checks if a `Char` is an upper case character. */
  case class UpperCase()

  /** Predicate that checks if a `Char` is white space. */
  case class Whitespace()

  /** Predicate that checks if a `Char` is a letter or digit. */
  type LetterOrDigit = Letter Or Digit
}

private[refined] trait CharInstances {

  implicit def digitValidate: Validate.Flat[Char, Digit] =
    Validate.fromPredicate(_.isDigit, Digit())

  implicit def digitShow: Show.Flat[Char, Digit] =
    Show.instance(t => s"isDigit('$t')")

  implicit def letterValidate: Validate.Flat[Char, Letter] =
    Validate.fromPredicate(_.isLetter, Letter())

  implicit def letterShow: Show.Flat[Char, Letter] =
    Show.instance(t => s"isLetter('$t')")

  implicit def lowerCaseValidate: Validate.Flat[Char, LowerCase] =
    Validate.fromPredicate(_.isLower, LowerCase())

  implicit def lowerCaseShow: Show.Flat[Char, LowerCase] =
    Show.instance(t => s"isLower('$t')")

  implicit def upperCaseValidate: Validate.Flat[Char, UpperCase] =
    Validate.fromPredicate(_.isUpper, UpperCase())

  implicit def upperCaseShow: Show.Flat[Char, UpperCase] =
    Show.instance(t => s"isUpper('$t')")

  implicit def whitespaceValidate: Validate.Flat[Char, Whitespace] =
    Validate.fromPredicate(_.isWhitespace, Whitespace())

  implicit def whitespaceShow: Show.Flat[Char, Whitespace] =
    Show.instance(t => s"isWhitespace('$t')")
}
