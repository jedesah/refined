package eu.timepit.refined.internal

import shapeless.{Nat, Succ, _0}

trait NatToValue[N <: Nat] extends Serializable {

  def toInt: Int

  def toNat: N
}

object NatToValue {

  def apply[N <: Nat](implicit ntv: NatToValue[N]): NatToValue[N] = ntv

  implicit val natToValue0: NatToValue[_0] =
    new NatToValue[_0] {
      def toInt: Int = 0

      def toNat: _0 = new _0
    }

  implicit def natToValueSucc[N <: Nat](implicit ntvN: NatToValue[N]): NatToValue[Succ[N]] =
    new NatToValue[Succ[N]] {
      def toInt: Int = ntvN.toInt + 1

      def toNat: Succ[N] = Succ[N]()
    }
}
