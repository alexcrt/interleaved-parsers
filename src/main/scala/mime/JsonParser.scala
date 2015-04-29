package mime

import java.io.Reader

import scala.util.parsing.combinator._

object JsonParser extends JavaTokenParsers {

    def parse(input: Reader) = parseAll(root, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
    }

    def root: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber ^^ (_.toDouble)|
                             "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)
    def obj: Parser[Map[String, Any]] = "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)
    def arr: Parser[List[Any]] = "["~> repsep(root, ",") <~"]"
    def member: Parser[(String, Any)] = stringLiteral~":"~ root ^^ {case name~":"~value => (name, value)}
}