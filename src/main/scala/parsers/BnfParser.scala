package parsers

import scala.util.parsing.combinator._
import java.io.Reader

object BnfParser extends RegexParsers {

  override def skipWhitespace = false
  
  def EOL = "\r\n" | "\n"
  def whitespace = " "
  def pipe = "|"
  def empty = ""
  def singleCharacters = """[.,]""".r
   
  def parse(input: Reader) = parseAll(syntax, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
  }
  
  def syntax: Parser[List[Rule]] = repsep(rule, EOL)
  def rule: Parser[Rule] = ('<' ~> ruleName <~ '>') ~ ((whitespace ~ "::=" ~ whitespace) ~> expression) map (x => Rule(x._1, x._2))
  def expression: Parser[List[Expression]] = repsep(term, whitespace ~ opt(pipe ~ whitespace))
  def term: Parser[Expression] = literal | '<' ~> ruleName <~ '>'
  def literal: Parser[TextExpression] = '"' ~> text <~ '"'
  def ruleName: Parser[RuleExpression] = """\w+""".r map (x => RuleExpression(x))
  def text: Parser[TextExpression] = ("""\w+""".r | singleCharacters | empty) map (x => TextExpression(x))
}


sealed abstract class BNFValue

case class Rule(rule: RuleExpression, expressions: List[Expression]) extends BNFValue {
  override def toString = rule + " ::= " + expressions.mkString(" ")
}

sealed abstract class Expression(name: String) extends BNFValue

case class TextExpression(name: String) extends Expression(name) {
  override def toString = "\"" + name + "\""
}
case class RuleExpression(name: String) extends Expression(name) {
  override def toString = "<" + name + ">"
}