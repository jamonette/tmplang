package com.tmp.lang.ast

sealed trait AST
case class Number(n: Int) extends AST
case class LList(l: List[AST]) extends AST
sealed trait Operator extends AST
case class Multiply() extends Operator
case class Add() extends Operator
case class Expression(o: Operator, l: LList) extends AST

