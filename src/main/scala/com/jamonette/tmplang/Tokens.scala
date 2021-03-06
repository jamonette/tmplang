package com.jamonette.tmplang

import scala.util.parsing.input.Positional

object Tokens {

  sealed trait Token extends Positional

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Text ////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////

  case class OPENPAREN() extends Token
  case class CLOSEPAREN() extends Token

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Special forms ///////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////

  case class LET() extends Token
  case class IF() extends Token
  case class FUNCTIONDEF() extends Token
  case class FUNCTIONCALL() extends Token
  case class VARIABLE(name: String) extends Token

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Value types /////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////

  case class NumberToken(n: Int) extends Token
  case class StringToken(s: String) extends Token
  case class TRUE() extends Token
  case class FALSE() extends Token

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Numerical operators /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////

  case class ADD() extends Token
  case class SUBTRACT() extends Token
  case class MULTIPLY() extends Token
  case class DIVIDE() extends Token

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// List operators //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////

  case class LIST() extends Token
  case class FIRST() extends Token
  case class REST() extends Token
  case class CONCAT() extends Token

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Comparison operators ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////

  case class EQUALS() extends Token

}

