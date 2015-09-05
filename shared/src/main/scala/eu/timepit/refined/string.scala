package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import shapeless.Witness

object string {

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

  object EndsWith {

    implicit def endsWithValidator[S <: String](implicit ws: Witness.Aux[S]): Validator.Flat[String, EndsWith[S]] =
      Validator.fromPredicate(_.endsWith(ws.value), EndsWith(ws.value))

    implicit def endsWithShow[S <: String](implicit ws: Witness.Aux[S]): Show.Flat[String, EndsWith[S]] =
      Show.instance(t => s""""$t".endsWith("${ws.value}")""")

    implicit def endsWithInference[A <: String, B <: String](
      implicit
      wa: Witness.Aux[A], wb: Witness.Aux[B]
    ): EndsWith[A] ==> EndsWith[B] =
      InferenceRule(wa.value.endsWith(wb.value), s"endsWithInference(${wa.value}, ${wb.value})")
  }

  object MatchesRegex {

    implicit def matchesRegexValidator[S <: String](implicit wr: Witness.Aux[S]): Validator.Flat[String, MatchesRegex[S]] =
      Validator.fromPredicate(_.matches(wr.value), MatchesRegex(wr.value))

    implicit def matchesRegexShow[S <: String](implicit ws: Witness.Aux[S]): Show.Flat[String, MatchesRegex[S]] =
      Show.instance(t => s""""$t".matches("${ws.value}")""")
  }

  object Regex {

    implicit def regexValidator: Validator.Flat[String, Regex] =
      Validator.fromPartial(new scala.util.matching.Regex(_), Regex())

    implicit def regexShow: Show.Flat[String, Regex] =
      Show.fromPartial(new scala.util.matching.Regex(_), "Regex")
  }

  object StartsWith {

    implicit def startsWithValidator[S <: String](implicit ws: Witness.Aux[S]): Validator.Flat[String, StartsWith[S]] =
      Validator.fromPredicate(_.startsWith(ws.value), StartsWith(ws.value))

    implicit def startsWithShow[S <: String](implicit ws: Witness.Aux[S]): Show.Flat[String, StartsWith[S]] =
      Show.instance(t => s""""$t".startsWith("${ws.value}")""")

    implicit def startsWithInference[A <: String, B <: String](
      implicit
      wa: Witness.Aux[A], wb: Witness.Aux[B]
    ): StartsWith[A] ==> StartsWith[B] =
      InferenceRule(wa.value.startsWith(wb.value), s"startsWithInference(${wa.value}, ${wb.value})")
  }

  object Uri {

    implicit def uriValidator: Validator.Flat[String, Uri] =
      Validator.fromPartial(new java.net.URI(_), Uri())

    implicit def uriShow: Show.Flat[String, Uri] =
      Show.fromPartial(new java.net.URI(_), "Uri")
  }

  object Url {

    implicit def urlValidator: Validator.Flat[String, Url] =
      Validator.fromPartial(new java.net.URL(_), Url())

    implicit def urlShow: Show.Flat[String, Url] =
      Show.fromPartial(new java.net.URL(_), "Url")
  }

  object Uuid {

    implicit def uuidValidator: Validator.Flat[String, Uuid] =
      Validator.fromPartial(java.util.UUID.fromString, Uuid())

    implicit def uuidShow: Show.Flat[String, Uuid] =
      Show.fromPartial(java.util.UUID.fromString, "Uuid")
  }

  object Xml {

    implicit def xmlValidator: Validator.Flat[String, Xml] =
      Validator.fromPartial(scala.xml.XML.loadString, Xml())

    implicit def xmlShow: Show.Flat[String, Xml] =
      Show.fromPartial(scala.xml.XML.loadString, "Xml")
  }

  object XPath {

    implicit def xpathValidator: Validator.Flat[String, XPath] =
      Validator.fromPartial(javax.xml.xpath.XPathFactory.newInstance().newXPath().compile, XPath())

    implicit def xpathShow: Show.Flat[String, XPath] =
      Show.fromPartial(javax.xml.xpath.XPathFactory.newInstance().newXPath().compile, "XPath")
  }
}
