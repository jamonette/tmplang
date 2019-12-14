package com.tmp.lang

import cats.Traverse
import cats.data.EitherT
import cats.data.State
import cats.implicits._
import com.tmp.lang.ast._

// We could create a map from the input text to AST
// at parse time to provide line number info for runtime errors
trait RuntimeError
case class BindingError(refName: String) extends RuntimeError
case class TypeError(msg: String) extends RuntimeError

case class InterpreterResult(output: Seq[String])

object Interpreter {

  type RuntimeStateMonad[A] = State[RuntimeState, A]
  type RuntimeMonad[A] = EitherT[RuntimeStateMonad, RuntimeError, A]

  case class RuntimeState(symbolTable: Map[Symbol, Expression])

  def dereference[T <: Expression](reference: Reference[T]): RuntimeMonad[Expression] =
    for {
      state <- EitherT.right(State.get[RuntimeState])
      res <-
        reference.value match {
          case Right(expression) =>
            EitherT.rightT(expression): RuntimeMonad[Expression]
          case Left(symbol) =>
            state.symbolTable.get(symbol) match {
              case Some(expression) => EitherT.rightT(expression): RuntimeMonad[Expression]
              case None =>
                val err = BindingError("No matching reference found for symbol: " + symbol.name)
                EitherT.leftT(err): RuntimeMonad[Expression]
            }
        }
    } yield res

  def addToSymbolTable(symbol: Symbol, expression: Expression): RuntimeMonad[Unit] =
    for {
      state <- EitherT.right(State.get[RuntimeState])
      newST = state.symbolTable + ((symbol, expression))
      newState = state.copy(symbolTable = newST)
      _ <- EitherT.right(State.set(newState))
    } yield ()

  def functionCall(fc: FunctionCall): RuntimeMonad[InterpreterResult] =
    for {
      functionExp <- dereference(fc.function)
      args <- dereference(fc.argument)

      // Ensure this expression is actually a function. Since lang
      // is dynamically typed, we can't encode this with types in the
      // interpreter and have have to check this at runtime.
      functionDef <- functionExp match {
        case fd: FunctionDef => EitherT.rightT(fd): RuntimeMonad[FunctionDef]
        case _ =>
          val symbolName = fc.function.value.left.getOrElse("[NOSYMBOL]")
          val err = TypeError("Symbol " + symbolName + " is not a function")
          EitherT.leftT(err): RuntimeMonad[FunctionDef]
      }

      functionBody <- dereference(functionDef.functionBody)

      // bind the function argument expression to the symbol
      // used for the formal parameter
      _ <- addToSymbolTable(functionDef.formalParameter, args)

      // evaluate the function body expression with the newly bound args
      res <- eval(functionBody)
    } yield res

  def withBinding(wb: With): RuntimeMonad[InterpreterResult] =
    for {
      valueToBind <- dereference(wb.valueToBind)
      toEvaluate <- dereference(wb.toEvaluate)
      symbol = wb.symbol
      _ <- addToSymbolTable(symbol, valueToBind)
      res <- eval(toEvaluate)
    } yield res

  val evalIf: (If) => RuntimeMonad[InterpreterResult] = ifExp =>
    for {
      cond <- dereference(ifExp.condition)
      c <- eval(cond)

      ifTrue <- dereference(ifExp.condition)
    } yield c

  def nonEmptyList(expList: NonEmptyListASTN): RuntimeMonad[InterpreterResult] = {
    val derefs: List[RuntimeMonad[Expression]] = expList.expressions.map(e => dereference(e)).toList
    val expCall: RuntimeMonad[List[Expression]] = Traverse[List].sequence[RuntimeMonad, Expression](derefs)

    for {
      expressions <- expCall
      _ <-
        if (expressions.isEmpty) {
          val err = TypeError("Expression list is empty. This should never happen and is an interpreter bug.")
          EitherT.leftT(err): RuntimeMonad[Unit]
        } else {
          EitherT.rightT(()): RuntimeMonad[Unit]
        }
      res <- EitherT.fromEither(listEval(expressions)): RuntimeMonad[InterpreterResult]
    } yield res
  }

  def binaryNumericalOperator(
    f: (Int, Int) => Int,
    operands: List[Expression]): Either[TypeError, InterpreterResult] = {

    val vals: Seq[Option[NumberLiteral]] = operands.map {
      case n: NumberLiteral => Some(n)
      case _ => None
    }

    if (vals.contains(None)) {
      Left(TypeError("Failed attempting to perform numerical op on non-number: " + operands.map(_.toString)))
    } else {
      Right(InterpreterResult(vals.flatten.map(_.n).reduce(f).toString :: Nil))
    }
  }

  def listEval(expList: List[Expression]): Either[RuntimeError, InterpreterResult] =
    expList match {
      //case Equals() :: rest =>
      case Add() :: rest => binaryNumericalOperator((a , b ) => a + b, rest)
      case Multiply() :: rest => binaryNumericalOperator((a , b ) => a * b, rest)
      case Subtract() :: rest => binaryNumericalOperator((a , b ) => a - b, rest)
      case Divide() :: rest => binaryNumericalOperator((a , b ) => a / b, rest)
      case rest =>
        if (rest == Nil) {
          Left(TypeError("List is Nil. This should never happen and is an interpreter error"))
        } else {
          Right(InterpreterResult(rest.map(_.toString)))
        }
    }

  def eval(expression: Expression): RuntimeMonad[InterpreterResult] = {
    val f: RuntimeMonad[InterpreterResult] = expression match {
      case f: FunctionCall => functionCall(f)
      case f: With => withBinding(f)
      case x: NonEmptyListASTN => nonEmptyList(x)
      case x: If => evalIf(x)
    }

    for {
      r <- f
    } yield r
  }

  def run(e: Expression): Either[RuntimeError, InterpreterResult] = {
    val runner =
      for {
        r <- eval(e)
      } yield r

    val (state, result)= runner.value.run(RuntimeState(Map())).value
    result
  }

}
