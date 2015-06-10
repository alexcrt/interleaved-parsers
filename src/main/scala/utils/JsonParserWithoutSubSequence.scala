package utils

import java.util.regex.Pattern

import scala.collection.immutable.ListMap
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader

object JsonParserWithoutSubSequence extends RegexParsers {

  def parse(input: java.io.Reader): Any = parse(root, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString)
  }

  def parse(input: Reader[Char]): Any = parse(root, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString)
  }

  def stringLiteral:Parser[String] = """"(\\.|[^"])*"""".r

  def decimalNumber:Parser[String] = "(\\-)?((\\d+(\\.\\d*)?)|(\\d*\\.\\d+))".r

  /** A parser that matches a literal string */
  override implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(s, in.drop(j - offset))
      else  {
        val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
        Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  def root: Parser[Any] = obj | arr

  def value: Parser[Any] =
    obj |
    arr |
    stringLiteral |
    decimalNumber  ^^ (_.toDouble) |
    "null" ^^ (x => null) |
    "true" ^^ (x => true) |
    "false" ^^ (x => false)

  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (ListMap() ++ _)

  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] = stringLiteral ~ ":" ~ value map { case name ~ ":" ~ value => (name, value) }
}