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
  
  def syntax: Parser[Syntax] = repsep(rule, EOL) map (list => Syntax(list))

  def rule: Parser[Rule] = ('<' ~> ruleName <~ '>') ~ ((whitespace ~> "::=" ~> whitespace) ~> expression) map (x => Rule(x._1, x._2))

  def expression: Parser[List[ExpressionSep]] = rep(term)

  def term: Parser[ExpressionSep] = (literal | '<' ~> ruleName <~ '>')  ~ ((opt(whitespace) ~> opt(pipe <~ whitespace)) map
                                    {
                                      case Some(p)=> p
                                      case None=> ""
                                    }) map (x => ExpressionSep(x._1, x._2))

  def literal: Parser[TextExpression] = '"' ~> text <~ '"'

  def ruleName: Parser[RuleExpression] = """\w+""".r map (x => RuleExpression(x))

  def text: Parser[TextExpression] = ("""\w+""".r | singleCharacters | empty) map (x => TextExpression(x))
}


sealed abstract class BNFValue

case class Syntax(rules: List[Rule]) extends BNFValue

case class Rule(rule: RuleExpression, expressions: List[BNFValue]) extends BNFValue {
  override def toString = rule + " ::= " + expressions.mkString
}
case class ExpressionSep (expr: Expression, separator: String) extends BNFValue {
  override def toString = expr + separator
}

sealed abstract class Expression(name: String) extends BNFValue

case class TextExpression(name: String) extends Expression(name) {
  override def toString = "\"" + name + "\""
}
case class RuleExpression(name: String) extends Expression(name) {
  override def toString = "<" + name + ">"
}