package com.jamonette.tmplang

import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("Lexer test") {
    val result = Parser.parse(ExpressionText.recursiveCall0)
    assert(result.toOption.get === ExpressionAST.recursiveCall0)
  }

  test("Value expression | string list") {
    val text = """(list "a" "b" "c")"""
    val result = Parser.parse(text)
    assert(result.toOption.get ===
      ListType(List(StringLiteral("a"), StringLiteral("b"), StringLiteral("c"))))
  }

  test("Value expression | mixed list | string, num") {
    val text = """(list "a" 1 "b" "c")"""
    val result = Parser.parse(text)
    assert(result.toOption.get ===
      ListType(List(StringLiteral("a"), NumberLiteral(1),StringLiteral("b"), StringLiteral("c"))))
  }

  test("Value expression | mixed list | string, num, var reference") {
    val text = """(list "a" 1 "b" some-var "c")"""
    val result = Parser.parse(text)
    assert(result.toOption.get ===
      ListType(List(StringLiteral("a"), NumberLiteral(1),StringLiteral("b"), VariableReference("some-var"), StringLiteral("c"))))
  }

}
