package parsers

import scala.util.parsing.combinator._
import java.io.Reader

object JsonParser extends JavaTokenParsers {

    def parse(input: Reader) = parseAll(value, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
    }

    def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber ^^ (_.toDouble)| 
                             "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)                             
    def obj: Parser[Map[String, Any]] = "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)
    def arr: Parser[List[Any]] = "["~> repsep(value, ",") <~"]"
    def member: Parser[(String, Any)] = stringLiteral~":"~value ^^ { case name~":"~value => (name, value)}
}