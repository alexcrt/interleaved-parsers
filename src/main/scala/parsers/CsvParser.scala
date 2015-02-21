package parsers

import scala.util.parsing.combinator._
import java.io.Reader

/**
 * CSV Parser based on RFC4180
 */
object CsvParser extends RegexParsers {

    override def skipWhitespace = false
    
    def CRLF = "\r\n" | "\n"
    def STRING_LITERAL = "[^\",\r\n]".r
    def DQUOTE = "\""
    def DQUOTE2 = "\"\""
    
    def parse(input: Reader) = parseAll(file, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
    }

    def file: Parser[List[List[String]]] = repsep(record, CRLF) <~ opt(CRLF)

    def record: Parser[List[String]] = repsep(field, ",")

    def field: Parser[String] = escaped | nonescaped
    
    def nonescaped: Parser[String] = rep(STRING_LITERAL) map(x => x.mkString)
    
    def escaped: Parser[String] = DQUOTE ~>(STRING_LITERAL | "," | CRLF | DQUOTE2)<~DQUOTE map(x => x.mkString)
  
}