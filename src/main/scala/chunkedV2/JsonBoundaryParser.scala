package chunkedV2

import java.io.Reader

import scala.util.parsing.combinator._

object JsonBoundaryParser extends MyRegexParsers {

    def parse(input: Reader) = parseAll(root, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
    }

    def character = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" |
                    "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" |
                    "w" | "x" | "y" | "z" | " "

    def string = "\"" ~> rep1(character) <~ "\"" map (l => l.mkString)

    def digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    def number = rep1(digit) map (l => l.mkString)

    def root: Parser[Any] = obj | arr | string | number ^^ (_.toInt)|
                             "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)
    def obj: Parser[Map[String, Any]] = "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)
    def arr: Parser[List[Any]] = "["~> repsep(root, ",") <~"]"
    def member: Parser[(String, Any)] = string ~ ":" ~ root ^^ {case name ~ ":" ~ value => (name, value)}
}