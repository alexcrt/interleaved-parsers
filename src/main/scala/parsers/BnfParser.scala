package parsers

import scala.util.parsing.combinator._
import java.io.Reader

object BnfParser extends RegexParsers {

  override def skipWhitespace = false
  
  def EOL = "\r\n" | "\n"
  def whitespace = " "
  def pipe = "|"
   
  def parse(input: Reader) = parseAll(syntax, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
  }
  
  def syntax: Parser[Any] = repsep(rule, EOL)
  def rule: Parser[Any] = "<" ~ ruleName ~ ">" <~ whitespace ~> "::=" <~ whitespace ~> expression
  def expression: Parser[Any] = repsep(term, whitespace ~ opt(pipe ~ whitespace))
  def term: Parser[Any] = literal | "<" ~ ruleName ~ ">"
  def literal: Parser[Any] = '"' ~ text ~ '"'
  def ruleName: Parser[Any] = """\w+""".r
  def text: Parser[Any] = """\w+""".r | """[.,]""".r | ""
}