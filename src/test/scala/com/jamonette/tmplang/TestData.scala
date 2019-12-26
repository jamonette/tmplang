package com.jamonette.tmplang

object ExpressionText {

  val recursiveCall0 =
    """
    (let message-0 "zero"
      (let message-1 "not zero"
        (let test-vals (list 0 1 2 0 4 5)
          (let demo-function
            (function-def input-values
              (let first-in-list (first (list input-values))
                (let rest-of-list (rest (list input-values))
                  (let first-msg
                    (if (= (list first-in-list 0))
                      (list message-0)
                      (list message-1))
                    (if (= (list rest-of-list ()))
                      first-msg
                      (let rest-msgs (function-call demo-function rest-of-list)
                        (concat (list first-msg rest-msgs))))))))
            (function-call demo-function test-vals)))))
    """

}

object ExpressionAST {

  val recursiveCall0 =
    Let("message-0", StringLiteral("zero"),
      Let("message-1", StringLiteral("not zero"),
        Let("test-vals", ListType(List(NumberLiteral(0), NumberLiteral(1), NumberLiteral(2), NumberLiteral(0), NumberLiteral(4), NumberLiteral(5))),
          Let("demo-function",
            FunctionDef("input-values",
              Let("first-in-list", OperatorCall(First(), ListType(List(VariableReference("input-values")))),
                Let("rest-of-list", OperatorCall(Rest(), ListType(List(VariableReference("input-values")))),
                  Let("first-msg",
                    If(OperatorCall(Equals(), ListType(List(VariableReference("first-in-list"), NumberLiteral(0)))),
                      ListType(List(VariableReference("message-0"))),
                      ListType(List(VariableReference("message-1")))),
                    If(OperatorCall(Equals(), ListType(List(VariableReference("rest-of-list"), ListType(List())))),
                      VariableReference("first-msg"),
                      Let("rest-msgs",
                        FunctionCall(VariableReference("demo-function"), VariableReference("rest-of-list")),
                        OperatorCall(Concat(), ListType(List(VariableReference("first-msg"), VariableReference("rest-msgs")))))))))),
            FunctionCall(VariableReference("demo-function"), VariableReference("test-vals"))))))

}
