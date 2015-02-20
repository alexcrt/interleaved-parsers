package parsers

import scala.util.parsing.combinator._
import java.io.Reader

class JsonParser(input: Reader) extends JavaTokenParsers {

    def parse(): ParseResult[Any] = {
        parseAll(value, input)
    }

    def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber ^^ (_.toDouble)| 
                             "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)                             
    def obj: Parser[Map[String, Any]] = "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)
    def arr: Parser[List[Any]] = "["~> repsep(value, ",") <~"]"
    def member: Parser[(String, Any)] = stringLiteral~":"~value ^^ { case name~":"~value => (name, value)}
}