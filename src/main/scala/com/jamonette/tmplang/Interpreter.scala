package com.jamonette.tmplang

import cats.Traverse
import cats.data.EitherT
import cats.data.State
import cats.implicits._

object Interpreter {

  private type RuntimeStateMonad[A] = State[RuntimeEnvironment, A]
  private type RuntimeMonad[A] = EitherT[RuntimeStateMonad, RuntimeError, A]

  // In this implementation, each "stack frame" is actually a full
  // copy of the environment, meaning it contains all variables in scope
  // (plus any that have been closed over).

  private case class RuntimeEnvironment(stack: Vector[Map[String, Evaluated]])

  def run(expression: ExpressionOrSpecialForm): Either[RuntimeError, Evaluated] = {
    val initialState = RuntimeEnvironment(Vector(Map()))
    val evalCall: RuntimeMonad[Evaluated] = eval(expression)
    val (finalState, result) = evalCall.value.run(initialState).value
    result
  }

  private def eval(expression: ExpressionOrSpecialFormOrReference): RuntimeMonad[Evaluated] =
    expression match {
      case e: FunctionCall => evalFunctionCall(e)
      case e: FunctionDef => evalFunctionDef(e)
      case e: If => evalIf(e)
      case e: Let => evalLetBinding(e)
      case e: ListType => evalList(e)
      case e: Operator => EitherT.rightT(EvaluatedOperator(e))
      case e: OperatorCall => evalOperatorCall(e)
      case e: Value => EitherT.rightT(EvaluatedValue(e))
      case e: VariableReference => dereferenceVariable(e)
    }

  private def evalFunctionCall(functionCall: FunctionCall): RuntimeMonad[Evaluated] =
    for {
      // evaluate the function argument
      argumentValue <- eval(functionCall.argument)

      functionDefExpression <- eval(functionCall.function)
      evaluatedFunctionDef <-
        (functionDefExpression match {
          case fdr: EvaluatedFunctionDef => EitherT.rightT(fdr)
          case _ => EitherT.fromEither(Left(TypeError("Function call must refer to a function definition")))
        }): RuntimeMonad[EvaluatedFunctionDef]

      preCallEnvironment <- EitherT.right(State.get[RuntimeEnvironment])

      // create a new stack frame and merge in the closed-over
      // values from the function definition site
      newFrame =
        evaluatedFunctionDef.lexicalEnvironment
          .foldLeft(preCallEnvironment.stack.head)((accum, mapEntry) => accum + mapEntry)

      stackWithNewFrame = preCallEnvironment.stack.prepended(newFrame)
      envWithNewStack = preCallEnvironment.copy(stack = stackWithNewFrame)
      _ <- EitherT.right(State.set(envWithNewStack))

      // bind the function argument to a variable with
      // the name of the functions formal parameter
      _ <- addToSymbolTable(evaluatedFunctionDef.functionDef.formalParameterName, argumentValue)

      // evaluate the function body in the new environment
      result <- eval(evaluatedFunctionDef.functionDef.functionBody)

      // pop frame
      postCallEnvironment <- EitherT.right(State.get[RuntimeEnvironment])
      envWithPoppedStack = postCallEnvironment.copy(stack = postCallEnvironment.stack.tail)
      _ <- EitherT.right(State.set(envWithPoppedStack))
    } yield result

  // Store the current stack frame in the EvaluatedFunctionDef.
  //
  // Later, when the function is called, merge any variables stored in the
  // FunctionDef into the current environment so that they are resolved in
  // preference to the stack at the call site. This allows for closures.

  private def evalFunctionDef(functionDef: FunctionDef): RuntimeMonad[Evaluated] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      stackFrame = environment.stack.head
    } yield EvaluatedFunctionDef(functionDef, stackFrame)

  private def addToSymbolTable(variableName: String, value: Evaluated): RuntimeMonad[Unit] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      currentStackFrame = environment.stack.head
      updatedFrame = currentStackFrame + ((variableName, value))
      newStack = environment.stack.tail.prepended(updatedFrame)
      newEnvironment = environment.copy(stack = newStack)
      _ <- EitherT.right(State.set(newEnvironment))
    } yield ()

  private def evalIf(ifForm: If): RuntimeMonad[Evaluated] =
    for {
      conditionValue <- eval(ifForm.condition)
      isTrue =
      conditionValue match {
        case EvaluatedValue(True()) => true
        case _ => false
      }
      branchEvalCall = if (isTrue) eval(ifForm.ifTrue) else eval(ifForm.ifFalse)

      result <- branchEvalCall
    } yield result

  private def evalLetBinding(letBinding: Let): RuntimeMonad[Evaluated] =
    for {
      valueToBind <- eval(letBinding.valueToBind)
      _ <- addToSymbolTable(letBinding.variableName, valueToBind)
      result <- eval(letBinding.toEvaluate)
    } yield result

  private def evalOperatorCall(operatorCall: OperatorCall): RuntimeMonad[Evaluated] =
    for {
      listValue <- operatorCall.list match {
        case l: ListType => eval(l)
        case r: VariableReference => dereferenceVariable(r)
      }
      list <- (listValue match {
        case EvaluatedList(list) => EitherT.rightT(list)
        case _ => EitherT.leftT(TypeError("Operator takes exactly one list as an argument"))
      }): RuntimeMonad[Seq[Evaluated]]

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
          EitherT.rightT(EvaluatedValue(result))

        ///////////////////////////////////////////////////////////////////////////////////////////
        ///// List operators /////////////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////////////////////////////////////

        case First() =>
          list match {
            case EvaluatedList(x) :: Nil => EitherT.rightT(x.head) // TODO: will throw on empty list
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

      }): RuntimeMonad[Evaluated]
    } yield result

  private def applyNumericOperatorToList(
    operatorFunc: (Int, Int) => Int,
    operands: Seq[Evaluated]): RuntimeMonad[Evaluated] = {

    val numbers: Seq[Option[Int]] = operands.map {
      case EvaluatedValue(NumberLiteral(n)) => Some(n)
      case _ => None
    }

    if (numbers.contains(None)) {
      val err = TypeError("Invalid input for binary numerical operator: " + operands.map(_.toString))
      EitherT.leftT(err)
    } else {
      val total = numbers.flatten.reduce((a, b) => operatorFunc(a, b))
      EitherT.rightT(EvaluatedValue(NumberLiteral(total)))
    }
  }

  private def compareExpressions(e1: Evaluated, e2: Evaluated): Int =
    (e1, e2) match {
      case (EvaluatedValue(v1), EvaluatedValue(v2)) =>
        (v1, v2) match {
          case (NumberLiteral(a), NumberLiteral(b)) => a.compareTo(b)
          case (True(), True()) => 0
          case (False(), False()) => 0
          case (True(), False()) => 1
          case (False(), True()) => -1
          case (StringLiteral(a), StringLiteral(b)) => a.compareTo(b)
          case _ => -1
        }
      case (EvaluatedList(l1), EvaluatedList(l2)) => if (l1 == l2) 0 else -1
      case _ => -1
    }

  private def dereferenceVariable(variableReference: VariableReference): RuntimeMonad[Evaluated] =
    for {
      environment <- EitherT.right(State.get[RuntimeEnvironment])
      value <-
        (environment.stack.head.get(variableReference.variableName) match {
          case Some(value) => EitherT.rightT(value)
          case None => EitherT.leftT(ReferenceError("Unable to dereference variable with name: " + variableReference.variableName))
        }): RuntimeMonad[Evaluated]
    } yield value

  private def evalList(list: ListType): RuntimeMonad[Evaluated] =
    for {
      concreteItems <- {
        val evalCalls: List[RuntimeMonad[Evaluated]] = list.items.map(i => eval(i)).toList
        Traverse[List].sequence[RuntimeMonad, Evaluated](evalCalls): RuntimeMonad[List[Evaluated]]
      }
    } yield EvaluatedList(concreteItems)

}

