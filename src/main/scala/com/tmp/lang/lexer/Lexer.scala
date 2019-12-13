package com.tmp.lang.lexer

import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {

  import com.tmp.lang.lexer.tokens._

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

