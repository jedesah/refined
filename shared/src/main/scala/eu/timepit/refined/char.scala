package eu.timepit.refined

import eu.timepit.refined.boolean.Or
import eu.timepit.refined.char._

object char extends CharValidators with CharShows {

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

private[refined] trait CharValidators {

  implicit def digitValidator: Validator.Flat[Char, Digit] =
    Validator.fromPredicate(_.isDigit, Digit())

  implicit def letterValidator: Validator.Flat[Char, Letter] =
    Validator.fromPredicate(_.isLetter, Letter())

  implicit def lowerCaseValidator: Validator.Flat[Char, LowerCase] =
    Validator.fromPredicate(_.isLower, LowerCase())

  implicit def upperCaseValidator: Validator.Flat[Char, UpperCase] =
    Validator.fromPredicate(_.isUpper, UpperCase())

  implicit def whitespaceValidator: Validator.Flat[Char, Whitespace] =
    Validator.fromPredicate(_.isWhitespace, Whitespace())
}

private[refined] trait CharShows {

  implicit def digitShow: Show[Char, Digit] =
    new Show[Char, Digit] {
      override def show(t: Char): String = s"isDigit('$t')"
    }

  implicit def letterShow: Show[Char, Letter] =
    new Show[Char, Letter] {
      override def show(t: Char): String = s"isLetter('$t')"
    }
}
