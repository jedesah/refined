package eu.timepit.refined

import java.time.LocalDateTime

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import monocle.Lens

object MonocleTest extends App {

  type Minute = Int Refined Interval.Closed[W.`0`.T, W.`59`.T]

  val minute = Lens[LocalDateTime, Minute](d => Refined.unsafeApply(d.getMinute))(m => d => d.withMinute(m.get))

  minute.set(10) // ok
  //minute.set(61) // does not compile

  // def indexLens(i: I): Lens[S, A]

}
