package chunked

import java.io.Reader

import scala.collection.immutable.ListMap

object JsonParser extends StringParsers {

    def parse(input: Reader): Any = parse(root, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString)
    }

    def root: Parser[Any] = obj | arr

    def value: Parser[Any] = obj |
                             arr |
                             stringLiteral |
                             decimalNumber ^^ (_.toDouble) |
                             "null" ^^ (x => null) |
                             "true" ^^ (x => true) |
                             "false" ^^ (x => false)

    def obj: Parser[Map[String, Any]] = "{" ~> w ~> repsep(member, w ~>","<~ w) <~ w <~"}" ^^ (ListMap() ++ _)

    def arr: Parser[List[Any]] = "["~> w ~> repsep(value, w ~>","<~ w) <~ w <~"]"

    def member: Parser[(String, Any)] = stringLiteral ~ w ~ ":" ~ w ~ value map {case name ~ w1 ~ ":" ~ w2 ~ value => (name, value)}
  def member2: Parser[(String, Any)] = (stringLiteral <~ w <~ ":" ) ~ (w ~> value) map {case name ~ value => (name, value)}
}