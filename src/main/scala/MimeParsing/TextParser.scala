package MimeParsing

import scala.util.parsing.combinator._

object TextParser extends RegexParsers {

    override def skipWhitespace = false

    def CRLF = "\r\n" | "\n"
    def TEXTDATA = ".*".r

    def takeUntil(condition: Parser[String], parser: Parser[String]): Parser[List[String]] = rep(not(condition) ~> parser)

    def root(boundary: Option[String] = None): Parser[List[String]] = boundary match {
        case Some(bound) => takeUntil("--frontier", TEXTDATA <~ CRLF)
        case None => repsep(TEXTDATA, CRLF)
    }
  
}