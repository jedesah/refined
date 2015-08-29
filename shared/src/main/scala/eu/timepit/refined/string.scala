package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.string._
import shapeless.Witness

object string extends StringValidators with StringInferenceRules {

  /** Predicate that checks if a `String` ends with the suffix `S`. */
  case class EndsWith[S](s: S)

  /** Predicate that checks if a `String` matches the regular expression `R`. */
  case class MatchesRegex[R](r: R)

  /** Predicate that checks if a `String` is a valid regular expression. */
  case class Regex()

  /** Predicate that checks if a `String` starts with the prefix `S`. */
  case class StartsWith[S](s: S)

  /** Predicate that checks if a `String` is a valid URI. */
  case class Uri()

  /** Predicate that checks if a `String` is a valid URL. */
  case class Url()

  /** Predicate that checks if a `String` is a valid UUID. */
  case class Uuid()

  /** Predicate that checks if a `String` is valid XML. */
  case class Xml()

  /** Predicate that checks if a `String` is a valid XPath expression. */
  case class XPath()
}

private[refined] trait StringValidators {

  implicit def endsWithValidator[S <: String](implicit ws: Witness.Aux[S]): Validator.Flat[String, EndsWith[S]] =
    Validator.fromPredicate(_.endsWith(ws.value), EndsWith(ws.value))

  implicit def matchesRegexValidator[R <: String](implicit wr: Witness.Aux[R]): Validator.Flat[String, MatchesRegex[R]] =
    Validator.fromPredicate(_.matches(wr.value), MatchesRegex(wr.value))

  implicit def regexValidator: Validator.Flat[String, Regex] =
    Validator.fromPartial(new scala.util.matching.Regex(_), Regex())

  implicit def startsWithValidator[S <: String](implicit ws: Witness.Aux[S]): Validator.Flat[String, StartsWith[S]] =
    Validator.fromPredicate(_.startsWith(ws.value), StartsWith(ws.value))

  implicit def uriValidator: Validator.Flat[String, Uri] =
    Validator.fromPartial(new java.net.URI(_), Uri())

  implicit def urlValidator: Validator.Flat[String, Url] =
    Validator.fromPartial(new java.net.URL(_), Url())

  implicit def uuidValidator: Validator.Flat[String, Uuid] =
    Validator.fromPartial(java.util.UUID.fromString, Uuid())

  implicit def xmlValidator: Validator.Flat[String, Xml] =
    Validator.fromPartial(scala.xml.XML.loadString, Xml())

  implicit def xpathValidator: Validator.Flat[String, XPath] =
    Validator.fromPartial(javax.xml.xpath.XPathFactory.newInstance().newXPath().compile, XPath())
}

private[refined] trait StringInferenceRules {

  implicit def endsWithInference[A <: String, B <: String](implicit wa: Witness.Aux[A], wb: Witness.Aux[B]): EndsWith[A] ==> EndsWith[B] =
    InferenceRule(wa.value.endsWith(wb.value), s"endsWithInference(${wa.value}, ${wb.value})")

  implicit def startsWithInference[A <: String, B <: String](implicit wa: Witness.Aux[A], wb: Witness.Aux[B]): StartsWith[A] ==> StartsWith[B] =
    InferenceRule(wa.value.startsWith(wb.value), s"startsWithInference(${wa.value}, ${wb.value})")
}
