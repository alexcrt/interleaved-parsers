package MimeParsing

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by alex on 15.04.15.
 */
trait TakeUntilParser extends RegexParsers {

    def CRLF = "\r\n" | "\n"
    override def skipWhitespace = false

    def takeUntil(condition: Parser[String], parser: Parser[String]): Parser[List[String]] = rep(not(condition) ~> parser)
}
