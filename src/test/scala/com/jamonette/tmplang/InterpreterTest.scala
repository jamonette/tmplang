package com.jamonette.tmplang

import org.scalatest.FunSuite
import com.jamonette.tmplang.ast._

class InterpreterTest extends FunSuite {

  test("Function call") {
    val ast =
      Let(
        VariableDefinition("func-1"),
        FunctionDef(
          VariableDefinition("input-var"),
          OperatorCall(
            Add(),
            UnevaluatedList(List(
              NumberLiteral(5),
              VariableReference("input-var"))))),
        FunctionCall(VariableReference("func-1"), NumberLiteral(10)))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === NumberLiteral(15))
  }

  test("Function call: recursive") {
    val ast =
      Let(VariableDefinition("msg0"), StringLiteral("it equals zero"),
        Let(VariableDefinition("msg1"), StringLiteral("it doesn't equal zero"),
          Let(VariableDefinition("test-vals"), UnevaluatedList(List(NumberLiteral(1), NumberLiteral(0), NumberLiteral(3), NumberLiteral(0))),
            // bind function to name 'is-zero'
            Let(VariableDefinition("is-zero"),
              FunctionDef(VariableDefinition("input-values"),
                Let(VariableDefinition("first"), OperatorCall(First(), UnevaluatedList(List(VariableReference("input-values")))),
                  Let(VariableDefinition("rest"), OperatorCall(Rest(), UnevaluatedList(List(VariableReference("input-values")))),
                    Let(VariableDefinition("result-of-first"),
                        If(OperatorCall(Equals(), UnevaluatedList(List(VariableReference("first"), NumberLiteral(0)))),
                          UnevaluatedList(List(VariableReference("msg0"))),
                          UnevaluatedList(List(VariableReference("msg1")))),
                        // if rest is empty, return first result, otherwise recurse
                        If(OperatorCall(Equals(), UnevaluatedList(List(VariableReference("rest"), UnevaluatedList(List())))),
                            VariableReference("result-of-first"),
                            Let(VariableDefinition("result-of-rest"),
                                FunctionCall(VariableReference("is-zero"), VariableReference("rest")),
                                OperatorCall(Concat(), UnevaluatedList(List(VariableReference("result-of-first"), VariableReference("result-of-rest")))))))))),
             FunctionCall(VariableReference("is-zero"), VariableReference("test-vals"))))))


    val result = Interpreter.run(ast)
    assert(result.toOption.get ===
      EvaluatedList(List(StringLiteral("it doesn't equal zero"), StringLiteral("it equals zero"), StringLiteral("it doesn't equal zero"), StringLiteral("it equals zero"))))
  }

  test("Operator: Numerical: Add") {
    val ast =
      OperatorCall(Add(), UnevaluatedList(List(NumberLiteral(400), NumberLiteral(300))))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === NumberLiteral(700))
  }

  test("Operator: Equals") {
    val ast =
      OperatorCall(Equals(), UnevaluatedList(List(True(), True())))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === True())
  }

  test("Operator: Equals 2") {
    val ast =
      OperatorCall(Equals(), UnevaluatedList(List(True(), True(), False())))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === False())
  }

  test("Operator: Equals 3") {
    val ast =
      OperatorCall(Equals(), UnevaluatedList(List(NumberLiteral(4), NumberLiteral(5))))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === False())
  }

  test("Operator: Equals 4") {
    val ast =
      OperatorCall(Equals(), UnevaluatedList(List(NumberLiteral(4), NumberLiteral(4))))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === True())
  }

  test("List Operators: First") {
    val ast =
      Let(
        VariableDefinition("the-list"),
        UnevaluatedList(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))),
        OperatorCall(First(), UnevaluatedList(List(VariableReference("the-list")))))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === NumberLiteral(1))
  }

  test("List Operators: Rest") {
    val ast =
      Let(
        VariableDefinition("the-list"),
        UnevaluatedList(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))),
        OperatorCall(Rest(), UnevaluatedList(List(VariableReference("the-list")))))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === EvaluatedList(List(NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))))
  }

  test("List Operators: Concat") {
    val ast =
      Let(
        VariableDefinition("list-1"),
        UnevaluatedList(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))),
          Let(
            VariableDefinition("list-2"),
            UnevaluatedList(List(StringLiteral("a"), StringLiteral("b"), StringLiteral("c"))),
            OperatorCall(Concat(), UnevaluatedList(List(VariableReference("list-1"), VariableReference("list-2"))))))

    val result = Interpreter.run(ast)
    assert(result.toOption.get ===
      EvaluatedList(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5), StringLiteral("a"), StringLiteral("b"), StringLiteral("c"))))
  }

  test("If: True") {
    val ast =
      Let(
        VariableDefinition("the-comparison-var"),
        True(),
        If(
          VariableReference("the-comparison-var"),
          StringLiteral("it's true"),
          StringLiteral("it's false")))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === StringLiteral("it's true"))
  }

  test("If: False") {
    val ast =
      Let(
        VariableDefinition("the-comparison-var"),
        False(),
        If(
          VariableReference("the-comparison-var"),
          StringLiteral("it's true"),
          StringLiteral("it's false")))

    val result = Interpreter.run(ast)
    assert(result.toOption.get === StringLiteral("it's false"))
  }

}
