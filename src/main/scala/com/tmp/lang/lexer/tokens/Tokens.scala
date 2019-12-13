package com.tmp.lang.lexer.tokens

sealed trait LangToken
case class NumberToken(n: Int) extends LangToken
case class MULTIPLY() extends LangToken
case class ADD() extends LangToken
case class OPENPAREN() extends LangToken
case class CLOSEPAREN() extends LangToken
case class WHITESPACE() extends LangToken

