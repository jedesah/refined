package eu.timepit.refined

import eu.timepit.refined.boolean.Or

object char {

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

  object Digit {

    implicit def digitValidator: Validator.Flat[Char, Digit] =
      Validator.fromPredicate(_.isDigit, Digit())

    implicit def digitShow: Show.Flat[Char, Digit] =
      Show.instance(t => s"isDigit('$t')")
  }

  object Letter {

    implicit def letterValidator: Validator.Flat[Char, Letter] =
      Validator.fromPredicate(_.isLetter, Letter())

    implicit def letterShow: Show.Flat[Char, Letter] =
      Show.instance(t => s"isLetter('$t')")
  }

  object LowerCase {

    implicit def lowerCaseValidator: Validator.Flat[Char, LowerCase] =
      Validator.fromPredicate(_.isLower, LowerCase())

    implicit def lowerCaseShow: Show.Flat[Char, LowerCase] =
      Show.instance(t => s"isLower('$t')")
  }

  object UpperCase {

    implicit def upperCaseValidator: Validator.Flat[Char, UpperCase] =
      Validator.fromPredicate(_.isUpper, UpperCase())

    implicit def upperCaseShow: Show.Flat[Char, UpperCase] =
      Show.instance(t => s"isUpper('$t')")
  }

  object Whitespace {

    implicit def whitespaceValidator: Validator.Flat[Char, Whitespace] =
      Validator.fromPredicate(_.isWhitespace, Whitespace())

    implicit def whitespaceShow: Show.Flat[Char, Whitespace] =
      Show.instance(t => s"isWhitespace('$t')")
  }
}
