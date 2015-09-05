package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.string._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class StringShowSpecJvm extends Properties("StringShow") {

  property("Regex.showResult.Failed") = secure {
    showResult[Regex]("(a|b") ?=
      """Regex predicate failed: Unclosed group near index 4
        |(a|b
        |    ^""".stripMargin
  }

  property("Url.showResult.Passed") = secure {
    showResult[Url]("http://example.com") ?= "Url predicate passed."
  }

  property("Url.showResult.Failed") = secure {
    showResult[Url]("htp://example.com") ?= "Url predicate failed: unknown protocol: htp"
  }

  property("Xml.showResult.Passed") = secure {
    showResult[Xml]("<root></root>") ?= "Xml predicate passed."
  }

  property("Xml.showResult.Failed") = secure {
    showResult[Xml]("<root>") ?=
      "Xml predicate failed: XML document structures must start and end within the same entity."
  }

  property("XPath.showResult.Passed") = secure {
    showResult[XPath]("A//B/*[1]") ?= "XPath predicate passed."
  }

  property("XPath.showResult.Failed") = secure {
    showResult[XPath]("A//B/*[1") ?=
      "XPath predicate failed: javax.xml.transform.TransformerException: Expected ], but found: "
  }
}
