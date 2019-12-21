package com.jamonette.tmplang

object TestImplicits {

  implicit class EvaluatedAsExpression(base: Evaluated) {
    def toExpression: Expression = toExpression(base)

    private def toExpression(evaluated: Evaluated): Expression =
      evaluated match {
        case EvaluatedValue(v) => v
        case EvaluatedList(items) =>
          val expressions: Seq[Expression] = items.map(i => toExpression(i))
          ListType(expressions)
        case _ => null
      }
  }

}
