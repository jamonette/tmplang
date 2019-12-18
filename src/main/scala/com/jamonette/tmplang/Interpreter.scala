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

sealed trait Result
case class FunctionDefResult(functionDef: FunctionDef, lexicalEnvironment: Map[String, Result]) extends Result
case class ListResult(items: List[Result]) extends Result
case class OperatorResult(operator: Operator) extends Result
case class ValueResult(value: Value) extends Result

object Interpreter {

  type RuntimeStateMonad[A] = State[RuntimeEnvironment, A]
  type RuntimeMonad[A] = EitherT[RuntimeStateMonad, RuntimeError, A]

  // TODO: add globals
  case class RuntimeEnvironment(
    stack: Vector[Map[String, Result]],
    lexicalEnvironment: Map[String, Result])

  def run(expression: ExpressionOrSpecialForm): Either[RuntimeError, Result] = {
    val initialState = RuntimeEnvironment(Vector(Map()), Map())
    val evalCall: RuntimeMonad[Result] = eval(expression)
    val (finalState, result) = evalCall.value.run(initialState).value
    result
  }

  private def eval(expression: ExpressionOrReferenceOrSpecialForm): RuntimeMonad[Result] =
    expression match {
      case e: FunctionCall => evalFunctionCall(e)
      case e: If => evalIf(e)
      case e: Let => evalLetBinding(e)
      case e: OperatorCall => evalOperatorCall(e)
      case e: VariableReference => dereferenceVariable(e)

      case e: FunctionDef => evalFunctionDef(e)

      case e: ListType => evalList(e)
      case e: Operator => EitherT.rightT(OperatorResult(e))
      case e: Value => EitherT.rightT(ValueResult(e))
    }

  private def evalFunctionCall(functionCall: FunctionCall): RuntimeMonad[Result] =
    for {
      // evaluate the function argument
      argumentValue <- eval(functionCall.argument)

      functionDefExpression <- eval(functionCall.function)
      functionDefResult <-
        (functionDefExpression match {
          case fdr: FunctionDefResult => EitherT.rightT(fdr)
          case _ => EitherT.fromEither(Left(TypeError("Function call must refer to a function definition")))
        }): RuntimeMonad[FunctionDefResult]

      // create a new stack frame
      preCallEnvironment <- EitherT.right(State.get[RuntimeEnvironment])

      // merge closed-over values from the function definition site into
      // the current stack frame
      newFrame =
        functionDefResult.lexicalEnvironment
          .foldLeft(preCallEnvironment.stack.head)((accum, mapEntry) => accum + mapEntry)

      stackWithNewFrame = preCallEnvironment.stack.prepended(newFrame)
      envWithNewStack = preCallEnvironment.copy(stack = stackWithNewFrame)
      _ <- EitherT.right(State.set(envWithNewStack))

      // bind the function argument to a variable with
      // the name of the functions formal parameter
      _ <- addToSymbolTable(functionDefResult.functionDef.formalParameter.variableName, argumentValue)

      // evaluate the function body in the new environment
      result <- eval(functionDefResult.functionDef.functionBody)

      // pop frame
      postCallEnvironment <- EitherT.right(State.get[RuntimeEnvironment])
      envWithPoppedStack = postCallEnvironment.copy(stack = postCallEnvironment.stack.tail)
      _ <- EitherT.right(State.set(envWithPoppedStack))
    } yield result

  // Store the current stack frame in the FunctionDefResult.
  // When the function is called, merge any variables stored in the
  // FunctionDef into the current environment so that they are resolved in
  // preference to the stack at the call site. Allows for closures.
  private def evalFunctionDef(functionDef: FunctionDef): RuntimeMonad[Result] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      stackFrame = environment.stack.head
    } yield FunctionDefResult(functionDef, stackFrame)

  private def addToSymbolTable(variableName: String, value: Result): RuntimeMonad[Unit] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      currentStackFrame = environment.stack.head
      updatedFrame = currentStackFrame + ((variableName, value))
      newStack = environment.stack.tail.prepended(updatedFrame)
      newEnvironment = environment.copy(stack = newStack)
      _ <- EitherT.right(State.set(newEnvironment))
    } yield ()

  private def evalIf(ifForm: If): RuntimeMonad[Result] =
    for {
      conditionValue <- eval(ifForm.condition)
      isTrue =
      conditionValue match {
        case ValueResult(True()) => true
        case _ => false
      }
      branchEvalCall = if (isTrue) eval(ifForm.ifTrue) else eval(ifForm.ifFalse)

      result <- branchEvalCall
    } yield result

  private def evalLetBinding(letBinding: Let): RuntimeMonad[Result] =
    for {
      valueToBind <- eval(letBinding.valueToBind)
      _ <- addToSymbolTable(letBinding.variable.variableName, valueToBind)
      result <- eval(letBinding.toEvaluate)
    } yield result

  private def evalOperatorCall(operatorCall: OperatorCall): RuntimeMonad[Result] =
    for {
      listValue <- operatorCall.list match {
        case l: ListType => eval(l)
        case r: VariableReference => dereferenceVariable(r)
      }
      list <- (listValue match {
        case ListResult(list) => EitherT.rightT(list)
        case _ => EitherT.leftT(TypeError("Operator takes exactly one list as an argument"))
      }): RuntimeMonad[Seq[Result]]

      result <-
      (operatorCall.operator match {

        ////////////////////////////////////////////////////////////////////////////////////////////
        ///// Numerical operators /////////////////////////////////////////////////////////////////
        //////////////////////////////////////////////////////////////////////////////////////////

        case Add() => applyNumericOperatorToList((a, b) => a + b, list)
        case Subtract() => applyNumericOperatorToList((a, b) => a - b, list)
        case Multiply() => applyNumericOperatorToList((a, b) => a * b, list)
        case Divide() => applyNumericOperatorToList((a, b) => a / b, list) // TODO: handle div0

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
          EitherT.rightT(ValueResult(result))

        ///////////////////////////////////////////////////////////////////////////////////////////
        ///// List operators /////////////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////////////////////////////////////

        case First() =>
          list match {
            case ListResult(x) :: Nil => EitherT.rightT(x.head) // TODO: will throw on empty list
            case _ => EitherT.leftT(TypeError("'First' takes exactly one list as an argument"))
          }
        case Rest() =>
          list match {
            case ListResult(x) :: Nil => EitherT.rightT(ListResult(x.tail))
            case _ => EitherT.leftT(TypeError("'Rest' takes exactly one list as an argument"))
          }
        case Concat() =>
          list match {
            case ListResult(l1) :: ListResult(l2) :: Nil => EitherT.rightT(ListResult(l1 ++ l2))
            case _ => EitherT.leftT(TypeError("'Concat' takes exactly two lists as arguments"))
          }

      }): RuntimeMonad[Result]
    } yield result

  private def applyNumericOperatorToList(
    operatorFunc: (Int, Int) => Int,
    operands: Seq[Result]): RuntimeMonad[Result] = {

    val numbers: Seq[Option[Int]] = operands.map {
      case ValueResult(NumberLiteral(n)) => Some(n)
      case _ => None
    }

    if (numbers.contains(None)) {
      val err = TypeError("Invalid input for binary numerical operator: " + operands.map(_.toString))
      EitherT.leftT(err)
    } else {
      val total = numbers.flatten.reduce((a, b) => operatorFunc(a, b))
      EitherT.rightT(ValueResult(NumberLiteral(total)))
    }
  }

  private def compareExpressions(e1: Result, e2: Result): Int =
    (e1, e2) match {
      case (ValueResult(v1), ValueResult(v2)) =>
        (v1, v2) match {
          case (NumberLiteral(a), NumberLiteral(b)) => a.compareTo(b)
          case (True(), True()) => 0
          case (False(), False()) => 0
          case (True(), False()) => 1
          case (False(), True()) => -1
          case (StringLiteral(a), StringLiteral(b)) => a.compareTo(b)
          case _ => -1
        }
      case (ListResult(l1), ListResult(l2)) => if (l1 == l2) 0 else -1
      case _ => -1
    }

  private def dereferenceVariable(variableReference: VariableReference): RuntimeMonad[Result] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      value <-
        (environment.stack.head.get(variableReference.variableName) match {
          case Some(value) => EitherT.rightT(value)
          case None => EitherT.leftT(ReferenceError("Unable to dereference variable with name: " + variableReference.variableName))
        }): RuntimeMonad[Result]
    } yield value

  private def evalList(list: ListType): RuntimeMonad[Result] =
    for {
      concreteItems <- {
        val evalCalls: List[RuntimeMonad[Result]] = list.items.map(i => eval(i)).toList
        Traverse[List].sequence[RuntimeMonad, Result](evalCalls): RuntimeMonad[List[Result]]
      }
    } yield ListResult(concreteItems)

}

