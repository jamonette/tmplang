package com.jamonette.tmplang.ast

sealed trait ExpressionOrSpecialForm
sealed trait ExpressionOrReference
sealed trait Expression extends ExpressionOrSpecialForm with ExpressionOrReference
sealed trait SpecialForm extends ExpressionOrSpecialForm

sealed trait Value extends Expression
sealed trait Operator extends Expression

////////////////////////////////////////////////////////////////////////////////////////////
///// Special forms ///////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

case class Let(variable: VariableDefinition, valueToBind: ExpressionOrSpecialForm, toEvaluate: ExpressionOrSpecialForm) extends SpecialForm
case class FunctionCall(function: ExpressionOrSpecialForm, argument: ExpressionOrSpecialForm) extends SpecialForm
case class If(condition: ExpressionOrSpecialForm, ifTrue: ExpressionOrSpecialForm, ifFalse: ExpressionOrSpecialForm) extends SpecialForm
case class VariableReference(variableName: String) extends SpecialForm
case class OperatorCall(operator: Operator, list: ExpressionOrSpecialForm) extends SpecialForm

case class FunctionDef(formalParameter: VariableDefinition, functionBody: ExpressionOrSpecialForm) extends Expression
case class VariableDefinition(variableName: String)

////////////////////////////////////////////////////////////////////////////////////////////
///// List types //////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

case class UnevaluatedList(items: Seq[ExpressionOrSpecialForm]) extends Expression
case class EvaluatedList(items: Seq[Expression]) extends Expression

////////////////////////////////////////////////////////////////////////////////////////////
///// Value types /////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

case class NumberLiteral(n: Int) extends Value
case class StringLiteral(s: String) extends Value

sealed trait BooleanValue extends Value
case class True() extends BooleanValue
case class False() extends BooleanValue

////////////////////////////////////////////////////////////////////////////////////////////
///// Numerical operators /////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

sealed trait NumericalOperator extends Operator
case class Multiply() extends NumericalOperator
case class Add() extends NumericalOperator
case class Subtract() extends NumericalOperator
case class Divide() extends NumericalOperator

////////////////////////////////////////////////////////////////////////////////////////////
///// List operators //////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

sealed trait ListOperator extends Operator
case class First() extends ListOperator
case class Rest() extends ListOperator
case class Concat() extends ListOperator

////////////////////////////////////////////////////////////////////////////////////////////
///// Comparison operators ////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

sealed trait ComparisonOperator extends Operator
case class Equals() extends ComparisonOperator

