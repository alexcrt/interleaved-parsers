package mime

import scala.util.parsing.combinator._



/**
 * CSV Parser based on RFC4180
 */
object CsvParser extends RegexParsers  {

    override def skipWhitespace = false
    
    def CRLF = "\r\n" | "\n"

    def TEXTDATA = """[^",\r\n]""".r
    def DQUOTE = "\""
    def DQUOTE2 = "\"\""

    def file: Parser[List[List[String]]] = repsep(record, CRLF) <~ opt(CRLF)

    def record: Parser[List[String]] = repsep(field, ",")

    def field: Parser[String] = escaped | nonescaped
    
    def nonescaped: Parser[String] = rep(TEXTDATA) map(x => x.mkString)
    
    def escaped: Parser[String] = DQUOTE ~>(TEXTDATA | "," | CRLF | DQUOTE2) <~ DQUOTE map(x => x.mkString)
  
}