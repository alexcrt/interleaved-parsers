package MimeParsing

import java.io.Reader

import scala.util.parsing.combinator._

object TextParser extends TakeUntilParser {

    def TEXTDATA = ".*".r

    def root(boundary: Option[String] = None): Parser[List[String]] = boundary match {
        case Some(bound) => takeUntil(rep(CRLF) ~> bound, TEXTDATA <~ CRLF)
        case None => repsep(TEXTDATA, CRLF)
    }
  
}