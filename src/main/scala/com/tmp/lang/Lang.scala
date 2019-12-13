package com.tmp.lang

import com.tmp.lang.ast._
import com.tmp.lang.lexer.Lexer
import com.tmp.lang.parser.LangParser

object Interpreter {

  def computeIt(ast: AST): Int =
    ast match {
      case Expression(operator, listNode) =>

        val listvals: Seq[Number] = for {
          astVal <- listNode.l
          numOpt = astVal match {
            case n: Number => Some(n)
            case _ => None
          }
          num <- numOpt
        } yield num 

        operator match {
          case Multiply() => listvals.map(_.n).reduce((n1, n2) => n1 * n2)
          case Add() => listvals.map(_.n).reduce((n1, n2) => n1 + n2)
          case _ => -1
        }

      case _ =>
        println("Bad AST, todo: exhaustive match + monadic EH") 
        -1
    }

}

// clean up clean up everybody

import scala.util.Success
object Lang extends App {

  val p1 =
    """(+
        (*
          (+ 4 9)))"""

  val p2 = "(+ 1 2 3 4 5 6)"

  val tokens = Lexer.run(p2)
  val ast = LangParser.p(tokens).get
  val r = Interpreter.computeIt(ast)
  println(ast + "\n-----\n" + r)
}



