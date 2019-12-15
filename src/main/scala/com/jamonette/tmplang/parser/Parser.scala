package com.jamonette.tmplang.parser

import com.jamonette.tmplang.ast._
import com.jamonette.tmplang.lexer.tokens._

import scala.util.parsing.combinator._
import scala.util.parsing.input._

// Work in progress, lexer and parser not yet implemented

object LangParser extends Parsers {

  override type Elem = LangToken

  class LangTokenReader(tokens: Seq[LangToken]) extends Reader[LangToken] {
    override def first: LangToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[LangToken] = new LangTokenReader(tokens.tail)
  }

  def number: Parser[NumberLiteral] = accept("number", { case num @ NumberToken(n) => NumberLiteral(n) })
  def multiply: Parser[Multiply] = MULTIPLY() ^^ (_ => Multiply())
  def add: Parser[Add] = ADD() ^^ (_ => Add())

}


