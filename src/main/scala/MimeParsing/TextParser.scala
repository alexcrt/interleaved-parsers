package MimeParsing

import scala.util.parsing.combinator._

trait TextParser extends RegexParsers {

    override def skipWhitespace = false

    def CRLF = "\r\n" | "\n"
    def TEXTDATA = ".*".r

    def text: Parser[List[String]] = repsep(TEXTDATA, CRLF)
  
}