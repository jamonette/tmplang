package com.jamonette.tmplang

import scala.io.StdIn
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object REPL extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- putStrLn(header)
      _ <- putStr(prompt)
      _ <- runRepl("", false)
    } yield ExitCode.Success

  private def runRepl(input: String, lastLineWasBlank: Boolean): IO[Unit] =
    if (lastLineWasBlank) {
      putStrLn(evalInput(input))
        .flatMap(_ => putStr(prompt))
        .flatMap(_ => runRepl("", false))
    } else {
      readLn.flatMap { newLine =>
        if (newLine.isEmpty) {
          runRepl(input, true)
        } else {
          runRepl(input + newLine, false)
        }
      }
    }

  private def evalInput(input: String): String =
    Parser.parse(input) match {
      case Left(GeneralParseError(errMsg)) => errMsg
      case Right(expression) =>
        Interpreter.run(expression) match {
          case Left(err: RuntimeError) => err.msg
          case Right(evaldExpression) => Evaluated.print(evaldExpression)
        }
      }

  private def putStr(s: String): IO[Unit] = IO { print(s) }
  private def putStrLn(s: String): IO[Unit] = IO { println(s) }
  private def readLn: IO[String] = IO { StdIn.readLine }

  private val header =
    """
      |/////////////////////////////////////////////////////////////////////
      |//// tmplang ///////////////////////////////////////////////////////
      |///////////////////////////////////////////////////////////////////
      |
      |Enter expression, hit return twice to eval.
      |""".stripMargin

  private val prompt = "tmplang>"

}

