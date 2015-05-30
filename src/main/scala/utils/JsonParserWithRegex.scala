package utils

import scala.util.parsing.input.Reader

import scala.collection.immutable.ListMap
import scala.util.parsing.combinator.RegexParsers

object JsonParserWithRegex extends RegexParsers {

  def parse(input: java.io.Reader): Any = parse(root, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString)
  }

  def parse(input: Reader[Char]): Any = parse(root, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString)
  }

  def stringLiteral = """"(\\.|[^"])*"""".r

  def decimalNumber = "(\\-)?((\\d+(\\.\\d*)?)|(\\d*\\.\\d+))".r

  def root: Parser[Any] = obj | arr

  def value: Parser[Any] =
    obj |
    arr |
    stringLiteral |
    decimalNumber ^^ (_.toDouble) |
    "null" ^^ (x => null) |
    "true" ^^ (x => true) |
    "false" ^^ (x => false)

  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (ListMap() ++ _)

  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] = stringLiteral ~ ":" ~ value map { case name ~ ":" ~ value => (name, value) }
}