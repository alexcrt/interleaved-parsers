package mime

import java.io.Reader

import scala.util.parsing.combinator._



/**
 * CSV Parser based on RFC4180
 */
object CsvParser extends RegexParsers {

    override def skipWhitespace = false
    
    def CRLF = "\r\n" | "\n"
    def TEXTDATA = """[^",\r\n]""".r
    def DQUOTE = "\""
    def DQUOTE2 = "\"\""

    def takeUntil(condition: Parser[String], parser: Parser[List[String]]): Parser[List[List[String]]] = rep(not(condition) ~> parser <~ CRLF)

    def file(boundary: Option[String] = None): Parser[List[List[String]]] = boundary match {
        case Some(bound) => takeUntil(rep(CRLF) ~> bound, record)
        case None => repsep(record, CRLF) <~ opt(CRLF)
    }

    def record: Parser[List[String]] = repsep(field, ",")

    def field: Parser[String] = escaped | nonescaped
    
    def nonescaped: Parser[String] = rep(TEXTDATA) map(x => x.mkString)
    
    def escaped: Parser[String] = DQUOTE ~>(TEXTDATA | "," | CRLF | DQUOTE2) <~ DQUOTE map(x => x.mkString)
  
}