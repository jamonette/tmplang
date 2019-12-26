package com.jamonette.tmplang

////////////////////////////////////////////////////////////////////////////////////////////
///// Interpreter errors //////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

sealed trait RuntimeError { def msg: String }
case class ReferenceError(msg: String) extends RuntimeError
case class TypeError(msg: String) extends RuntimeError

////////////////////////////////////////////////////////////////////////////////////////////
///// Interpreter result //////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

// These are wrapper types that allow the interpreter to distinguish between
// evaluated and un-evaluated expressions (and in the case of ListType, sub-expressions).
//
// Also provides a convenient place to store runtime information like the lexical
// environment at the time of function definition.

sealed trait Evaluated
case class EvaluatedFunctionDef(functionDef: FunctionDef, lexicalEnvironment: Map[String, Evaluated]) extends Evaluated
case class EvaluatedList(items: List[Evaluated]) extends Evaluated
case class EvaluatedOperator(operator: Operator) extends Evaluated
case class EvaluatedValue(value: Value) extends Evaluated

object Evaluated {
  def print(evaluated: Evaluated): String =
    evaluated match {
      case EvaluatedList(items) => "(" + items.map(print).mkString(", ") + ")"
      case EvaluatedValue(StringLiteral(s)) => s
      case EvaluatedValue(NumberLiteral(n)) => n.toString
      case e => e.toString
    }
}

////////////////////////////////////////////////////////////////////////////////////////////
///// Parse errors ////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

sealed trait ParseError { def msg: String }
case class GeneralParseError(msg: String) extends ParseError

