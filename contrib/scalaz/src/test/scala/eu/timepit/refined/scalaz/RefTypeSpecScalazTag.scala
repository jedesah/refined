package eu.timepit.refined.scalaz

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.api.{ RefType, RefTypeSpec }
import eu.timepit.refined.numeric._
import eu.timepit.refined.scalaz.auto._
import org.scalacheck.Prop._
import shapeless.test.illTyped

import _root_.scalaz.@@

class RefTypeSpecScalazTag extends RefTypeSpec[@@]("scalaz.@@") {

  property("refineM with type alias") = secure {
    type PositiveInt = Int @@ Positive

    val x: PositiveInt = RefType[@@].refineM(5)
    val y: PositiveInt = 5
    val z = 5: PositiveInt
    illTyped("val a: PositiveInt = -5", "Predicate failed: \\(-5 > 0\\).*")
    x == y && y == z
  }

  property("(T @@ P) <!: T") = wellTyped {
    illTyped("implicitly[(Int @@ Positive) <:< Int]", "Cannot prove.*")
  }
}
