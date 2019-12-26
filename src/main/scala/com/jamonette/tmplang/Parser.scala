package com.jamonette.tmplang

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import com.jamonette.tmplang.Tokens._

object Parser extends Parsers {

  override type Elem = Token

  private class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }

  def parse(input: String): Either[ParseError, ExpressionOrSpecialForm] =
    for {
      tokens <- Lexer(input)
      result <-
        expressionOrSpecialForm(new TokenReader(tokens)) match {
          case Success(expression, _) => Right(expression)
          case NoSuccess(err, nextToken) =>
            val msg = s"$err occurring at line ${nextToken.pos.line} | col ${nextToken.pos.column}"
            Left(GeneralParseError(msg))
        }
    } yield result

  private def expressionOrSpecialForm: Parser[ExpressionOrSpecialForm] =
    functionCall | ifParser | let | operatorCall | variableReference | functionDef | list | value | operator

  private def functionCall: Parser[FunctionCall] =
    // pass a FunctionDef directly to a FunctionCall
    OPENPAREN() ~ FUNCTIONCALL() ~ functionDef ~ expressionOrSpecialForm ~ CLOSEPAREN() ^^ {
      case _ ~ _ ~ (fd @ FunctionDef(_, _)) ~ argument ~ _ => FunctionCall(fd, argument)
    } |
    // call a function via a variable reference to the FunctionDef
    OPENPAREN() ~ FUNCTIONCALL() ~ variableToken ~ expressionOrSpecialForm ~ CLOSEPAREN() ^^ {
      case _ ~ _ ~ VARIABLE(funcName) ~ argument ~ _ => FunctionCall(VariableReference(funcName), argument)
    }

  private def ifParser: Parser[If] =
    OPENPAREN() ~ IF() ~ expressionOrSpecialForm ~ expressionOrSpecialForm ~ expressionOrSpecialForm ~ CLOSEPAREN() ^^ {
      case _ ~ _ ~ condition ~ toBind ~ toEval ~ _ => If(condition, toBind, toEval)
    }

  private def let: Parser[Let] =
    OPENPAREN() ~ LET() ~ variableToken ~ expressionOrSpecialForm ~ expressionOrSpecialForm ~ CLOSEPAREN() ^^ {
      case _ ~ _ ~ VARIABLE(varName) ~ toBind ~ toEval ~ _ => Let(varName, toBind, toEval)
    }

  private def operatorCall: Parser[OperatorCall] =
    OPENPAREN() ~ operator ~ (list | variableReference) ~ CLOSEPAREN() ^^ {
      case _ ~ operator ~ listOrRef ~ _ => OperatorCall(operator, listOrRef)
    }

  private def variableReference: Parser[VariableReference] = accept("variable", { case VARIABLE(varName) => VariableReference(varName) })

  private def functionDef: Parser[FunctionDef] =
    OPENPAREN() ~ functionDefToken ~ variableToken ~ expressionOrSpecialForm ~ CLOSEPAREN() ^^ {
      case _ ~ FUNCTIONDEF() ~ VARIABLE(paramName) ~ body ~ _ => FunctionDef(paramName, body)
    }

  private def list: Parser[ListType] =
    // empty list
    OPENPAREN() ~ CLOSEPAREN() ^^ { _ => ListType(List()) } |
    // non-empty list
    OPENPAREN() ~ LIST() ~ rep1(expressionOrSpecialForm) ~ CLOSEPAREN() ^^ {
      case _ ~ _ ~ items ~ _ => ListType(items.toList)
    }

  private def value: Parser[Value] =
    accept("number", { case NumberToken(n) => NumberLiteral(n) }) |
    accept("string", { case StringToken(s) => StringLiteral(s) }) |
    accept("true", { case TRUE() => True() }) |
    accept("false", { case FALSE() => False() })

  private def operator: Parser[Operator] =
    accept("add", { case ADD() => Add() }) |
    accept("subtract", { case SUBTRACT() => Subtract() }) |
    accept("multiply", { case MULTIPLY() => Multiply() }) |
    accept("divide", { case DIVIDE() => Divide() }) |
    listOperator |
    comparisonOperator

  private def listOperator: Parser[ListOperator] =
    accept("first", { case FIRST() => First() }) |
    accept("rest", { case REST() => Rest() }) |
    accept("concat", { case CONCAT() => Concat() })

  private def comparisonOperator: Parser[ComparisonOperator] =
    accept("equals", { case EQUALS() => Equals() })

  private def variableToken: Parser[VARIABLE] = accept("variable", { case v: VARIABLE => v })
  private def functionDefToken: Parser[FUNCTIONDEF] = accept("function def", { case f: FUNCTIONDEF => f })

}

