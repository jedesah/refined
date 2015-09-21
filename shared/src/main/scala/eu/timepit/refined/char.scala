package eu.timepit.refined

import eu.timepit.refined.api.{Show, Validate}
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.char._

object char extends CharPredicates with CharInstances {

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

private[refined] trait CharPredicates {

  implicit def letterPredicate: Predicate[Letter, Char] =
    Predicate.instance(_.isLetter, t => s"isLetter('$t')")

  implicit def lowerCasePredicate: Predicate[LowerCase, Char] =
    Predicate.instance(_.isLower, t => s"isLower('$t')")

  implicit def upperCasePredicate: Predicate[UpperCase, Char] =
    Predicate.instance(_.isUpper, t => s"isUpper('$t')")

  implicit def whitespacePredicate: Predicate[Whitespace, Char] =
    Predicate.instance(_.isWhitespace, t => s"isWhitespace('$t')")
}

private[refined] trait CharInstances {

  implicit def digitValidate: Validate.Flat[Char, Digit] =
    Validate.fromPredicate(_.isDigit, Digit())

  implicit def digitShow: Show.Flat[Char, Digit] =
    Show.instance(t => s"isDigit('$t')")
}
