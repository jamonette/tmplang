package com.jamonette.tmplang

import scala.util.parsing.combinator.RegexParsers
import com.jamonette.tmplang.Tokens._

object Lexer extends RegexParsers {

  def apply(expression: String): Either[ParseError, Seq[Token]] =
    parse(tokens, expression) match {
      case Success(result, _) => Right(result)
      case NoSuccess(err, nextToken) =>
        val msg = s"$err occurring at line ${nextToken.pos.line} | col ${nextToken.pos.column}"
        Left(ParseError(msg))
    }

  private def openParen: Parser[OPENPAREN] = "\\(".r ^^ { _ => OPENPAREN() }
  private def closeParen: Parser[CLOSEPAREN] = "\\)".r ^^ { _ => CLOSEPAREN() }
  private def let: Parser[LET] = Strings.let.r ^^ { _ => LET() }
  private def ifToken: Parser[IF] = Strings.ifStr.r ^^ { _ => IF() }
  private def functionDef: Parser[FUNCTIONDEF] = Strings.functionDef.r ^^ { _ => FUNCTIONDEF() }
  private def functionCall: Parser[FUNCTIONCALL] = Strings.functionCall.r ^^ { _ => FUNCTIONCALL() }
  private def number: Parser[NumberToken] = """0|[1-9]\d*""".r ^^ { n => NumberToken(n.toInt) }
  private def string: Parser[StringToken] = """".*"""".r ^^ { s => StringToken(s.substring(1, s.length - 1)) }
  private def trueToken: Parser[TRUE] = Strings.trueStr.r ^^ { _ => TRUE() }
  private def falseToken: Parser[FALSE] = Strings.falseStr.r ^^ { _ => FALSE() }
  private def add: Parser[ADD] = """\+""".r ^^ { _ => ADD() }
  private def subtract: Parser[SUBTRACT] = """\-""".r ^^ { _ => SUBTRACT() }
  private def multiply: Parser[MULTIPLY] = """\*""".r ^^ { _ => MULTIPLY() }
  private def divide: Parser[DIVIDE] = """\/""".r ^^ { _ => DIVIDE() }
  private def list: Parser[LIST] = Strings.list.r ^^ { _ => LIST() }
  private def first: Parser[FIRST] = Strings.first.r ^^ { _ => FIRST() }
  private def rest: Parser[REST] = Strings.rest.r ^^ { _ => REST() }
  private def concat: Parser[CONCAT] = Strings.concat.r ^^ { _ => CONCAT() }
  private def equals: Parser[EQUALS] = "=".r ^^ { _ => EQUALS() }
  private def variable: Parser[VARIABLE] =
    ({ """([a-z]+([a-z0-9]*-[a-z0-9]+)*)""".r ^^ { n => VARIABLE(n) }})
      .withFilter(v => !Strings.reservedWords.contains(v.name))

  private def tokens: Parser[Seq[Token]] =
    phrase(rep1(
      openParen |
      closeParen |
      let |
      ifToken |
      functionDef |
      functionCall |
      variable |
      number |
      string |
      trueToken |
      falseToken |
      add |
      subtract |
      multiply |
      divide |
      list |
      first |
      rest |
      concat |
      equals
    )) ^^ { t => t }

  object Strings {
    val let = "let"
    val ifStr = "if"
    val functionDef = "function-def"
    val functionCall = "function-call"
    val trueStr = "TRUE"
    val falseStr = "FALSE"
    val list = "list"
    val first = "first"
    val rest = "rest"
    val concat = "concat"

    // REFACTOR: might be cool to generate `reservedWords`
    // with a macro so it can't fall out of sync with the
    // object members

    def reservedWords: Set[String] =
      Set(let, ifStr, functionDef, functionCall, trueStr,
        falseStr, list, first, rest, concat)
  }

}

