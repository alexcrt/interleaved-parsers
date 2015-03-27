package MimeParsing

import scala.util.parsing.combinator._

object TextParser extends RegexParsers {

    override def skipWhitespace = false

    def CRLF = "\r\n" | "\n"
    def TEXTDATA = ".*".r

    def root(boundary: Option[String] = None): Parser[List[String]] = repsep(TEXTDATA, CRLF)
  
}