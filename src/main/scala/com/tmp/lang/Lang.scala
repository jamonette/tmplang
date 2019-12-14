package com.tmp.lang

import com.tmp.lang.ast._

object Lang extends App {

  import com.tmp.lang.ast.AstImplicits._

  val exampleProgramString =
    """
      (with msg0 "it equals zero"
        (with msg1 "it doesn't equal zero"
          (with test-vals (1 2 3)
            (with is-zero
              (func input-values
                (with i (first input-values)
                  (insert
                    (call is-zero (rest input-values))
                    (if (= i 0)
                      msg0
                      msg1)))))))
              (call is-zero test-vals))
    """
  val simpleProgram =
    With(
      Symbol("func-1"),
      FunctionDef(
        Symbol("input-var"),
        NonEmptyListASTN(List(
          Add(),
          NumberLiteral(5),
          SymbolDeref(Symbol("input-var"))))),
      FunctionCall(SymbolDeref(Symbol("func-1")), NumberLiteral(10)))


  val exampleProgramAst =
    With(Symbol("msg0"), StringLiteral("it equals zero"),
      With(Symbol("msg1"), StringLiteral("it doesn't equal zero"),
        With(Symbol("test-vals"), NonEmptyListASTN(List(NumberLiteral(1), NumberLiteral(2), NumberLiteral(3))),
          With(Symbol("is-zero"),
            FunctionDef(Symbol("input-values"),
              With(Symbol("i"), First(SymbolDeref(Symbol("input-values"))),
                Insert(
                  FunctionCall(SymbolDeref(Symbol("is-zero")), Rest(SymbolDeref(Symbol("input-values")))),
                  If(
                    NonEmptyListASTN(List(
                      Equals(),
                      SymbolDeref(Symbol("i")),
                      NumberLiteral(0))),
                    SymbolDeref(Symbol("msg0")),
                    SymbolDeref(Symbol("msg1")))))),
      FunctionCall(SymbolDeref(Symbol("is-zero")), SymbolDeref(Symbol("test-vals")))))))


  
  println(Interpreter.run(simpleProgram))
  //println(Interpreter.run(exampleProgramAst))

}



