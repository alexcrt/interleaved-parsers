package parsers

import scala.util.parsing.combinator._
import java.io.Reader

object BnfParser extends RegexParsers {
  
  override def skipWhitespace = false
  
  def EOL = "\r\n" | "\n"
   
  def parse(input: Reader) = parseAll(syntax, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
  }
  
  def syntax: Parser[Any] = rule | rule ~ syntax
  def rule: Parser[Any] = optWhitespace~"<" ~ruleName~">"~optWhitespace~"::="~optWhitespace~expression~lineEnd
  def optWhitespace: Parser[Any] = " "~optWhitespace | ""
  def expression: Parser[Any] = list | list~"|"~expression
  def lineEnd: Parser[Any] = optWhitespace~EOL | lineEnd~lineEnd
  def list: Parser[Any] = term | term~optWhitespace~list
  def term: Parser[Any] = literal | "<"~ruleName~">"
  def literal: Parser[Any] = '"'~text~"'" | "'"~text~"'"
  def ruleName: Parser[Any] = """\w+""".r
  def text: Parser[Any] = """\w+""".r
  
}