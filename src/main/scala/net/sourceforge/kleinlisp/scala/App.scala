package net.sourceforge.kleinlisp.scala

import net.sourceforge.kleinlisp.Lisp

import net.sourceforge.kleinlisp.scala.LispObj._

/**
  * @author ${user.name}
  */
object App {

  def main(args: Array[String]) {

    val lisp = new Lisp
    lisp.environment().define(
      "PLUS",
      (l: List[LispObj]) => l match {
        case IntObj(i) :: IntObj(j) :: Nil => IntObj(i + j)
        case _ => ErrorObj()
      }
    )

    println(lisp.evaluate("(PLUS 1 2)"))
  }


}
