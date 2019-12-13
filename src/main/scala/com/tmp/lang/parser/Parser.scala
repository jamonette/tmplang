package com.tmp.lang.parser

import scala.util.parsing._
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import com.tmp.lang.ast._

object LangParser extends Parsers {
  import com.tmp.lang.lexer.tokens._

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


