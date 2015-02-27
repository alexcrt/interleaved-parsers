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
  
  def syntax: Parser[String] = repsep(rule, EOL) map (x => x.mkString("\n"))
  def rule: Parser[Map[String, List[String]]] = ('<' ~> ruleName <~ '>') ~ ((whitespace ~ "::=" ~ whitespace) ~> expression) map (x => Map(x._1 -> x._2))
  def expression: Parser[List[String]] = repsep(term, whitespace ~ opt(pipe ~ whitespace))
  def term: Parser[String] = literal | ('<' ~> ruleName <~ '>' map (x  => "<"+x+">"))
  def literal: Parser[String] = '"' ~> text <~ '"' map (x => "\""+x+"\"")
  def ruleName: Parser[String] = """\w+""".r
  def text: Parser[String] = """\w+""".r | singleCharacters | empty
}