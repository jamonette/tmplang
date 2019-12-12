import scala.util.parsing.combinator._

trait Symbol

sealed trait LangToken
case class Number(n: Int) extends LangToken
case class MULTIPLY() extends LangToken 
case class ADD() extends LangToken
case class OPENPAREN() extends LangToken
case class CLOSEPAREN() extends LangToken
case class WHITESPACE() extends LangToken

object Lexer extends RegexParsers {
  def number: Parser[Number] = { """0|[1-9]\d*""".r ^^ { n => Number(n.toInt) } }
  def multiply: Parser[MULTIPLY] = { "\\*".r ^^ { _ => MULTIPLY() }}
  def add: Parser[ADD] = { "\\+".r ^^ { _ => ADD() }}
  def openParen: Parser[OPENPAREN] = { "\\(".r ^^ { _ => OPENPAREN() }}
  def closeParen: Parser[CLOSEPAREN] = { "\\)".r ^^ { _ => CLOSEPAREN() }}
  def whitespace: Parser[WHITESPACE] = { "\\s".r ^^ { _ => WHITESPACE() }}

  def tokens2: Parser[List[LangToken]] =
    phrase(rep1(
      number |
      multiply |
      add |
      openParen |
      closeParen |
      whitespace)) ^^ { t => t } 

  //def tokens = phrase(rep1(whitespace | number))

  def run(code: String) = 
    parse(tokens2, code) match {
      case Success(r, _) => r
      case _ => null
    }

}

object Lang extends App {

  val program =
    """(+
        (*
          (+ 4 9)))"""



  val r = Lexer.run(program)
  println(r)
}



