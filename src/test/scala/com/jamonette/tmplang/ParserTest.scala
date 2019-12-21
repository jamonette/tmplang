package com.jamonette.tmplang

import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  private def parse(input: String): ExpressionOrSpecialForm =
    (for {
      tokens <- Lexer(input)
      expression <- Parser(tokens)
    } yield expression).toOption.get

  test("Lexer test") {
    assert(parse(ExpressionText.recursiveCall0) === ExpressionAST.recursiveCall0)
  }

}
