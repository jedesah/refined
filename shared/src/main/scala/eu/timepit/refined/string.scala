package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.api.{Show, Validate}
import eu.timepit.refined.string._
import shapeless.Witness

object string extends StringInstances with StringInferenceRules {

  /** Predicate that checks if a `String` ends with the suffix `S`. */
  case class EndsWith[S](s: S)

  /** Predicate that checks if a `String` matches the regular expression `R`. */
  case class MatchesRegex[S](s: S)

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

private[refined] trait StringInstances {

  implicit def endsWithValidate[S <: String](implicit ws: Witness.Aux[S]): Validate.Flat[String, EndsWith[S]] =
    Validate.fromPredicate(_.endsWith(ws.value), EndsWith(ws.value))

  implicit def endsWithShow[S <: String](implicit ws: Witness.Aux[S]): Show.Flat[String, EndsWith[S]] =
    Show.instance(t => s""""$t".endsWith("${ws.value}")""")

  implicit def matchesRegexValidate[S <: String](implicit ws: Witness.Aux[S]): Validate.Flat[String, MatchesRegex[S]] =
    Validate.fromPredicate(_.matches(ws.value), MatchesRegex(ws.value))

  implicit def matchesRegexShow[S <: String](implicit ws: Witness.Aux[S]): Show.Flat[String, MatchesRegex[S]] =
    Show.instance(t => s""""$t".matches("${ws.value}")""")

  implicit def regexValidate: Validate.Flat[String, Regex] =
    Validate.fromPartial(new scala.util.matching.Regex(_), Regex())

  implicit def regexShow: Show.Flat[String, Regex] =
    Show.fromPartial(new scala.util.matching.Regex(_), "Regex")

  implicit def startsWithValidate[S <: String](implicit ws: Witness.Aux[S]): Validate.Flat[String, StartsWith[S]] =
    Validate.fromPredicate(_.startsWith(ws.value), StartsWith(ws.value))

  implicit def startsWithShow[S <: String](implicit ws: Witness.Aux[S]): Show.Flat[String, StartsWith[S]] =
    Show.instance(t => s""""$t".startsWith("${ws.value}")""")

  implicit def uriValidate: Validate.Flat[String, Uri] =
    Validate.fromPartial(new java.net.URI(_), Uri())

  implicit def uriShow: Show.Flat[String, Uri] =
    Show.fromPartial(new java.net.URI(_), "Uri")

  implicit def urlValidate: Validate.Flat[String, Url] =
    Validate.fromPartial(new java.net.URL(_), Url())

  implicit def urlShow: Show.Flat[String, Url] =
    Show.fromPartial(new java.net.URL(_), "Url")

  implicit def uuidValidate: Validate.Flat[String, Uuid] =
    Validate.fromPartial(java.util.UUID.fromString, Uuid())

  implicit def uuidShow: Show.Flat[String, Uuid] =
    Show.fromPartial(java.util.UUID.fromString, "Uuid")

  implicit def xmlValidate: Validate.Flat[String, Xml] =
    Validate.fromPartial(scala.xml.XML.loadString, Xml())

  implicit def xmlShow: Show.Flat[String, Xml] =
    Show.fromPartial(scala.xml.XML.loadString, "Xml")

  implicit def xpathValidate: Validate.Flat[String, XPath] =
    Validate.fromPartial(javax.xml.xpath.XPathFactory.newInstance().newXPath().compile, XPath())

  implicit def xpathShow: Show.Flat[String, XPath] =
    Show.fromPartial(javax.xml.xpath.XPathFactory.newInstance().newXPath().compile, "XPath")
}

private[refined] trait StringInferenceRules {

  implicit def endsWithInference[A <: String, B <: String](implicit wa: Witness.Aux[A], wb: Witness.Aux[B]): EndsWith[A] ==> EndsWith[B] =
    InferenceRule(wa.value.endsWith(wb.value), s"endsWithInference(${wa.value}, ${wb.value})")

  implicit def startsWithInference[A <: String, B <: String](implicit wa: Witness.Aux[A], wb: Witness.Aux[B]): StartsWith[A] ==> StartsWith[B] =
    InferenceRule(wa.value.startsWith(wb.value), s"startsWithInference(${wa.value}, ${wb.value})")
}
