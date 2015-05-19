package chunkedV2

import java.io.Reader

object JsonBoundaryParser extends StringParser {

    def parse(input: Reader): Any = parse(root, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
    }
    val x = "\"x\""

    def root: Parser[Any] = obj | arr | stringLiteral | integerNumber ^^ (_.toInt) |
                             "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)

    def obj: Parser[Map[String, Any]] = "{" ~> w ~> repsep(member, w ~>","<~ w) <~ w <~"}" ^^ (Map() ++ _)

    def arr: Parser[List[Any]] = "["~> w ~> repsep(root, w ~>","<~ w) <~ w <~"]"

    def member: Parser[(String, Any)] = (stringLiteral ~ w ~ ":") ~ w ~ root map {case name ~ w1 ~ ":" ~ w2 ~ value => (name, value)}
}