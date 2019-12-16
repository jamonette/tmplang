package com.jamonette.tmplang

import cats.Traverse
import cats.data.EitherT
import cats.data.State
import cats.implicits._
import com.jamonette.tmplang.ast._

trait RuntimeError
case class InterpreterError(msg: String) extends RuntimeError
case class ReferenceError(msg: String) extends RuntimeError
case class TypeError(msg: String) extends RuntimeError

object Interpreter {

  type RuntimeStateMonad[A] = State[RuntimeEnvironment, A]
  type RuntimeMonad[A] = EitherT[RuntimeStateMonad, RuntimeError, A]

  case class RuntimeEnvironment(symbolTable: Map[String, ExpressionOrReference])

  def run(expression: ExpressionOrSpecialForm): Either[RuntimeError, Expression] = {
    val initialState = RuntimeEnvironment(Map())
    val evalStep: RuntimeMonad[Expression] = eval(expression)
    val (finalState, result) = evalStep.value.run(initialState).value
    result
  }

  private def eval(expression: ExpressionOrSpecialForm): RuntimeMonad[Expression] =
    expression match {
      case e: Value => EitherT.rightT(e)
      case e: Operator => EitherT.rightT(e)
      case e: UnevaluatedList => evalListAsExpression(e)
      case e: EvaluatedList => EitherT.rightT(e)
      case e: FunctionCall => evalFunctionCall(e)
      case e: Let => evalLetBinding(e)
      case e: If => evalIf(e)
      case e: VariableReference => dereferenceVariable(e)
      case e: OperatorCall => evalOperatorCall(e)
      case e: FunctionDef => EitherT.rightT(e)
    }

  private def dereferenceVariable[T <: Expression](variableReference: VariableReference): RuntimeMonad[Expression] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      value <-
        (environment.symbolTable.get(variableReference.variableName) match {
          case Some(value) => EitherT.rightT(value)
          case None => EitherT.leftT(ReferenceError("Unable to dereference variable with name: " + variableReference.variableName))
        }): RuntimeMonad[ExpressionOrReference]
      result <-
        (value match {
          case r: VariableReference => dereferenceVariable(r)
          case e: Expression => EitherT.rightT(e)
        }): RuntimeMonad[Expression]
    } yield result

  private def evalFunctionCall(functionCall: FunctionCall): RuntimeMonad[Expression] =
    for {
      // evaluate the function argument
      argumentValue <- eval(functionCall.argument)

      functionDefExpression <- eval(functionCall.function)
      functionDef <-
        (functionDefExpression match {
          case f: FunctionDef => EitherT.rightT(f)
          case _ => EitherT.fromEither(Left(TypeError("Function call must refer to a function definition")))
        }): RuntimeMonad[FunctionDef]

      // bind the function argument to a variable with
      // the name of the functions formal parameter
      _ <- addToSymbolTable(functionDef.formalParameter.variableName, argumentValue)

      // evaluate the function body in the new environment
      result <- eval(functionDef.functionBody)
    } yield result

  private def addToSymbolTable(variableName: String, value: ExpressionOrReference): RuntimeMonad[Unit] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      newSymbolTable = environment.symbolTable + ((variableName, value))
      newEnvironment = environment.copy(symbolTable = newSymbolTable)
      _ <- EitherT.right(State.set(newEnvironment))
    } yield ()

  private def evalLetBinding(letBinding: Let): RuntimeMonad[Expression] =
    for {
      valueToBind <- eval(letBinding.valueToBind)
      _ <- addToSymbolTable(letBinding.variable.variableName, valueToBind)
      result <- eval(letBinding.toEvaluate)
    } yield result

  private def evalIf(ifForm: If): RuntimeMonad[Expression] =
    for {
      conditionValue <- eval(ifForm.condition)
      isTrue =
        conditionValue match {
          case True() => true
          case _ => false
        }
      branchEvalCall = if (isTrue) eval(ifForm.ifTrue) else eval(ifForm.ifFalse)

      result <- branchEvalCall
    } yield result

  private def evalList(list: UnevaluatedList): RuntimeMonad[EvaluatedList] =
    for {
      evaldItems <- {
        val evalCalls: List[RuntimeMonad[Expression]] = list.items.map(i => eval(i)).toList
        Traverse[List].sequence[RuntimeMonad, Expression](evalCalls): RuntimeMonad[List[Expression]]
      }
    } yield EvaluatedList(evaldItems)

  // hack around what I think is a variance issue w/ State type
  private def evalListAsExpression(list: UnevaluatedList): RuntimeMonad[Expression] =
    evalList(list).map(evaluatedList => (evaluatedList: Expression))

  private def evalOperatorCall(operatorCall: OperatorCall): RuntimeMonad[Expression] =
    for {
      opCallArgumentExpression <- eval(operatorCall.list)
      list <- (opCallArgumentExpression match {
        case l: EvaluatedList => EitherT.rightT(l.items)
        case _ => EitherT.leftT(TypeError("Operator takes exactly one list as an argument"))
      }): RuntimeMonad[Seq[Expression]]

      result <-
      (operatorCall.operator match {

        ////////////////////////////////////////////////////////////////////////////////////////////
        ///// Numerical operators /////////////////////////////////////////////////////////////////
        //////////////////////////////////////////////////////////////////////////////////////////

        case Add() => applyNumericOperatorToList((a, b) => a + b, list)
        case Multiply() => applyNumericOperatorToList((a, b) => a * b, list)
        case Subtract() => applyNumericOperatorToList((a, b) => a - b, list)
        case Divide() =>  applyNumericOperatorToList((a, b) => a / b, list) // TODO handle div0

        ///////////////////////////////////////////////////////////////////////////////////////////
        ///// Comparison operators ///////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////////////////////////////////////

        case op: Equals =>

          // TODO: unsafe, will throw exception on list len < 2
          val allListItemsEqual =
            list.sliding(2, 1).toList
              .map(pair => compareExpressions(pair.head, pair.last) == 0)
              .reduce((a, b) => a && b)

          val result = if (allListItemsEqual) True() else False()
          EitherT.rightT(result)

        ///////////////////////////////////////////////////////////////////////////////////////////
        ///// List operators /////////////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////////////////////////////////////

        case First() =>
          list match {
            case EvaluatedList(x) :: Nil => EitherT.rightT(x.head) // TODO will throw on empty list
            case _ => EitherT.leftT(TypeError("'First' takes exactly one list as an argument"))
          }
        case Rest() =>
          list match {
            case EvaluatedList(x) :: Nil => EitherT.rightT(EvaluatedList(x.tail))
            case _ => EitherT.leftT(TypeError("'Rest' takes exactly one list as an argument"))
          }
        case Concat() =>
          list match {
            case EvaluatedList(l1) :: EvaluatedList(l2) :: Nil => EitherT.rightT(EvaluatedList(l1 ++ l2))
            case _ => EitherT.leftT(TypeError("'Concat' takes exactly two lists as arguments"))
          }

      }): RuntimeMonad[Expression]
    } yield result

  private def applyNumericOperatorToList(
    operatorFunc: (Int, Int) => Int,
    operands: Seq[Expression]): RuntimeMonad[Expression] = {

    val numberLiterals: Seq[Option[NumberLiteral]] = operands.map {
      case n: NumberLiteral => Some(n)
      case _ => None
    }

    if (numberLiterals.contains(None)) {
      val err = TypeError("Invalid input for binary numerical operator: " + operands.map(_.toString))
      EitherT.fromEither(Left(err))
    } else {
      val total = numberLiterals.flatten.map(_.n).reduce((a, b) => operatorFunc(a, b))
      EitherT.fromEither(Right(NumberLiteral(total)))
    }
  }

  private def compareExpressions(e1: Expression, e2: Expression): Int =
    // TODO: method for testing equality of lists, other types
    (e1, e2) match {
      case (NumberLiteral(a), NumberLiteral(b)) => a.compareTo(b)
      case (True(), True()) => 0
      case (False(), False()) => 0
      case (True(), False()) => 1
      case (False(), True()) => -1
      case (StringLiteral(a), StringLiteral(b)) => a.compareTo(b)
      case (e1, e2) => if (e1 == e2) 0 else -1
      case _ => 0
    }
}
