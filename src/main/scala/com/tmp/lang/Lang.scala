import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait LangToken extends Positional
case class NumberToken(n: Int) extends LangToken
case class MULTIPLY() extends LangToken 
case class ADD() extends LangToken
case class OPENPAREN() extends LangToken
case class CLOSEPAREN() extends LangToken
case class WHITESPACE() extends LangToken

object Lexer extends RegexParsers {
  def number: Parser[NumberToken] = { """0|[1-9]\d*""".r ^^ { n => NumberToken(n.toInt) } }
  def multiply: Parser[MULTIPLY] = { "\\*".r ^^ { _ => MULTIPLY() }}
  def add: Parser[ADD] = { "\\+".r ^^ { _ => ADD() }}
  def openParen: Parser[OPENPAREN] = { "\\(".r ^^ { _ => OPENPAREN() }}
  def closeParen: Parser[CLOSEPAREN] = { "\\)".r ^^ { _ => CLOSEPAREN() }}
  def whitespace: Parser[WHITESPACE] = { "\\s".r ^^ { _ => WHITESPACE() }}

  def tokens: Parser[List[LangToken]] =
    phrase(rep1(
      number |
      multiply |
      add |
      openParen |
      closeParen |
      whitespace)) ^^ { t => t } 

  def run(code: String) = 
    parse(tokens, code) match {
      case Success(r, _) => r
      case _ => null
    }

}

sealed trait AST 
case class Number(n: Int) extends AST
case class LList(l: List[AST]) extends AST
sealed trait Operator extends AST
case class Multiply() extends Operator
case class Add() extends Operator
case class Expression(o: Operator, l: LList) extends AST

object LangParser extends Parsers {
  override type Elem = LangToken

  class LangTokenReader(tokens: Seq[LangToken]) extends Reader[LangToken] {
    override def first: LangToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[LangToken] = new LangTokenReader(tokens.tail)
  }

  def number: Parser[Number] = accept("number", { case num @ NumberToken(n) => Number(n) })
  
  def multiply: Parser[Multiply] = MULTIPLY() ^^ (_ => Multiply())
  def add: Parser[Add] = ADD() ^^ (_ => Add())

  def operator: Parser[Operator] = (multiply | add) ^^ (o => o)

  def list: Parser[LList] =
    (WHITESPACE().? ~ number ~ WHITESPACE().?).* ^^ 
      { case instances =>
        val nums: List[Number] = instances.map(i => i._1._2)
        LList(nums)
      }

  def expression: Parser[Expression] = OPENPAREN() ~ operator ~ list ~ CLOSEPAREN() ^^ { case _ ~ op ~ lst ~ _ => Expression(op, lst) }


  def p(tokens: Seq[LangToken]) = expression(new LangTokenReader(tokens))

}

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



