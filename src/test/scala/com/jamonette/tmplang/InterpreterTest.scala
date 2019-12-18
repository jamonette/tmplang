package com.jamonette.tmplang

import org.scalatest.FunSuite
import com.jamonette.tmplang.ast._
import scala.language.implicitConversions

object ResultImplicits {
  implicit def resultToExpression(result: Result): Expression =
    result match {
      case ValueResult(v) => v
      case ListResult(items) =>
        val expressions: Seq[Expression] = items.map(i => resultToExpression(i))
        ListType(expressions)
      case _ => null
    }
}

class InterpreterTest extends FunSuite {

  import ResultImplicits._

  test("Function call") {
    val ast =
      Let(
        VariableDefinition("func-1"),
        FunctionDef(
          VariableDefinition("input-var"),
          OperatorCall(
            Add(),
            ListType(List(
              NumberLiteral(5),
              VariableReference("input-var"))))),
        FunctionCall(VariableReference("func-1"), NumberLiteral(10)))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === NumberLiteral(15))
  }

  test("Function call: recursive") {
    val ast =
      Let(VariableDefinition("msg0"), StringLiteral("it equals zero"),
        Let(VariableDefinition("msg1"), StringLiteral("it doesn't equal zero"),
          Let(VariableDefinition("test-vals"), ListType(List(NumberLiteral(1), NumberLiteral(0), NumberLiteral(3), NumberLiteral(0))),
            // bind function to name 'is-zero'
            Let(VariableDefinition("is-zero"),
              FunctionDef(VariableDefinition("input-values"),
                Let(VariableDefinition("first"), OperatorCall(First(), ListType(List(VariableReference("input-values")))),
                  Let(VariableDefinition("rest"), OperatorCall(Rest(), ListType(List(VariableReference("input-values")))),
                    Let(VariableDefinition("result-of-first"),
                        If(OperatorCall(Equals(), ListType(List(VariableReference("first"), NumberLiteral(0)))),
                          ListType(List(VariableReference("msg0"))),
                          ListType(List(VariableReference("msg1")))),
                        // if rest is empty, return first result, otherwise recurse
                        If(OperatorCall(Equals(), ListType(List(VariableReference("rest"), ListType(List())))),
                            VariableReference("result-of-first"),
                            Let(VariableDefinition("result-of-rest"),
                                FunctionCall(VariableReference("is-zero"), VariableReference("rest")),
                                OperatorCall(Concat(), ListType(List(VariableReference("result-of-first"), VariableReference("result-of-rest")))))))))),
             FunctionCall(VariableReference("is-zero"), VariableReference("test-vals"))))))


  val result: Expression = Interpreter.run(ast).toOption.get
  assert(result ===
    ListType(List(StringLiteral("it doesn't equal zero"), StringLiteral("it equals zero"), StringLiteral("it doesn't equal zero"), StringLiteral("it equals zero"))))
  }

  test("Function call: closure") {
    val ast =
      Let(VariableDefinition("closed-var-1"), NumberLiteral(1),
        Let(VariableDefinition("func-generator"),
          FunctionDef(VariableDefinition("func-generator-args"),
            Let(VariableDefinition("closed-var-1"), NumberLiteral(2),
              Let(VariableDefinition("closed-var-2"), StringLiteral("asdf"),
                FunctionDef(VariableDefinition("the-func-args"),
                  ListType(List(VariableReference("closed-var-1"), VariableReference("closed-var-2"))))))),
          Let(
            VariableDefinition("the-func"),
            FunctionCall(VariableReference("func-generator"), ListType(List())),
            FunctionCall(VariableReference("the-func"), ListType(List())))))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === ListType(List(NumberLiteral(2), StringLiteral("asdf"))))
  }

  test("Operator: Numerical: Add") {
    val ast =
      OperatorCall(Add(), ListType(List(NumberLiteral(400), NumberLiteral(300))))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === NumberLiteral(700))
  }

  test("Operator: Equals") {
    val ast =
      OperatorCall(Equals(), ListType(List(True(), True())))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === True())
  }

  test("Operator: Equals 2") {
    val ast =
      OperatorCall(Equals(), ListType(List(True(), True(), False())))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === False())
  }

  test("Operator: Equals 3") {
    val ast =
      OperatorCall(Equals(), ListType(List(NumberLiteral(4), NumberLiteral(5))))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === False())
  }

  test("Operator: Equals 4") {
    val ast =
      OperatorCall(Equals(), ListType(List(NumberLiteral(4), NumberLiteral(4))))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === True())
  }

  test("List Operators: First") {
    val ast =
      Let(
        VariableDefinition("the-list"),
        ListType(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))),
        OperatorCall(First(), ListType(List(VariableReference("the-list")))))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === NumberLiteral(1))
  }

  test("List Operators: Rest") {
    val ast =
      Let(
        VariableDefinition("the-list"),
        ListType(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))),
        OperatorCall(Rest(), ListType(List(VariableReference("the-list")))))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === ListType(List(NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))))
  }

  test("List Operators: Concat") {
    val ast =
      Let(
        VariableDefinition("list-1"),
        ListType(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5))),
          Let(
            VariableDefinition("list-2"),
            ListType(List(StringLiteral("a"), StringLiteral("b"), StringLiteral("c"))),
            OperatorCall(Concat(), ListType(List(VariableReference("list-1"), VariableReference("list-2"))))))

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result ===
      ListType(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3), NumberLiteral(4), NumberLiteral(5), StringLiteral("a"), StringLiteral("b"), StringLiteral("c"))))
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

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === StringLiteral("it's true"))
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

    val result: Expression = Interpreter.run(ast).toOption.get
    assert(result === StringLiteral("it's false"))
  }

}
