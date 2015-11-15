package eu.timepit.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import monocle._

trait SafeAt[S, A] {
  type I
  def at(i: I): Lens[S, A]
}

trait Alias {
  type ZeroTo[T] = Int Refined Interval.Closed[W.`0`.T, T]
}

object SafeAt extends Alias {
  type Aux[S, A, I0] = SafeAt[S, A] { type I = I0 }

  def apply[S, A](implicit il: SafeAt[S, A]): Aux[S, A, il.I] = il

  def instance[S, A, I0](get: I0 => S => A)(set: I0 => A => S => S): Aux[S, A, I0] =
    new SafeAt[S, A] {
      override type I = I0
      override def at(i: I): Lens[S, A] = Lens(get(i))(set(i))
    }

  implicit val byteSafeAt: SafeAt.Aux[Byte, Boolean, ZeroTo[W.`7`.T]] =
    instance[Byte, Boolean, ZeroTo[W.`7`.T]] { i => s =>
      ((s >> i.get) & 1) == 1
    } { i => a => s =>
      val mask = 1 << i.get
      if (a) (s | mask).toByte
      else (s & ~mask).toByte
    }
}

/*
Usage:

scala> SafeAt[Byte,Boolean].at(0).get(6)
res0: Boolean = false

scala> SafeAt[Byte,Boolean].at(0).get(7)
res1: Boolean = true

scala> SafeAt[Byte,Boolean].at(0).get(8)
res2: Boolean = false

scala> SafeAt[Byte,Boolean].at(0).set(true)(8)
res3: Byte = 9

scala> SafeAt[Byte,Boolean].at(1).set(true)(8)
res4: Byte = 10

scala> SafeAt[Byte,Boolean].at(7).set(true)(8)
res5: Byte = -120

scala> SafeAt[Byte,Boolean].at(7).set(false)(8)
res6: Byte = 8

scala> SafeAt[Byte,Boolean].at(10).set(true)(8)
<console>:47: error: Right predicate of (!(10 < 0) && !(10 > 7)) failed: Predicate (10 > 7) did not fail.
       SafeAt[Byte,Boolean].at(10).set(true)(8)
                               ^
*/
