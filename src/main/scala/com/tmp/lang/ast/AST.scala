package com.tmp.lang.ast

import scala.language.implicitConversions

case class Reference[T <: Expression](value: Either[Symbol, T])

object AstImplicits {
  implicit def concreteToReference[T <: Expression](value: T): Reference[T] = Reference(Right(value))
  implicit def symbolToReference[T <: Expression](value: SymbolDeref): Reference[T] = Reference(Left(value.symbol))
}

sealed trait AST

case class Symbol(name: String) extends AST

sealed trait Expression extends AST

sealed trait ListASTN extends Expression
case class EmptyList() extends ListASTN
case class NonEmptyListASTN(expressions: Seq[Reference[Expression]]) extends ListASTN

sealed trait Value extends Expression
sealed trait LiteralValue extends Value
case class NumberLiteral(n: Int) extends LiteralValue
case class StringLiteral(s: String) extends LiteralValue
case class BooleanLiteral() extends LiteralValue

case class SymbolDeref(symbol: Symbol) extends Expression
case class With(symbol: Symbol, valueToBind: Reference[Expression], toEvaluate: Reference[Expression]) extends Expression

trait FunctionASTN extends Expression
case class FunctionDef(formalParameter: Symbol, functionBody: Reference[Expression]) extends FunctionASTN
case class FunctionCall(function: Reference[FunctionDef], argument: Reference[Expression]) extends Expression

case class If(condition: Reference[Expression], ifTrue: Reference[Expression], ifFalse: Reference[Expression]) extends Expression

sealed trait Operator extends Expression
case class Equals() extends Operator
case class Multiply() extends Operator
case class Add() extends Operator
case class Subtract() extends Operator
case class Divide() extends Operator

// Change these to operators
case class First(list: Reference[Expression]) extends Expression
case class Rest(list: Reference[Expression]) extends Expression
case class Insert(list: Reference[Expression], toInsert: Reference[Expression]) extends Expression

