package eu.timepit.refined
package internal

import scala.reflect.macros.blackbox
import scala.util.{Success, Try}

object MacroUtils {

  def eval[T](c: blackbox.Context)(t: c.Expr[T]): T = {
    import c.universe._
    println("eval begin")
    println("t: " + showRaw(t))
    val expr = c.Expr[T](c.untypecheck(t.tree))
    println("expr: " + showRaw(expr))
    // Try evaluating expr twice before failing, see
    // https://github.com/fthomas/refined/issues/3
    val r = tryN(2, c.eval(expr))
    println("eval end")
    r
  }

  def tryN[T](n: Int, t: => T): T = {
    println("tryN begin")

    val rr = Stream.fill(n)(Try(t)).map(x => {
      println(x); x
    }).collect { case Success(r) => r }.headOption.getOrElse(t)

    println("tryN end")
    rr
  }
}
