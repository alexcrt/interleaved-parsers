package chunkedV2

import java.io.Reader

object JsonBoundaryParser extends MyRegexParsers {

    def parse(input: Reader): Any = parse(root, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
    }

    def character = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" |
                    "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" |
                    "w" | "x" | "y" | "z" |
                    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" |
                    "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" |
                    "W" | "X" | "Y" | "Z" |
                    "@" | "-" | "." | " "


    def string: Parser[String] = "\"" ~> rep1(character | digit) <~ "\"" map (l => l.mkString)

    def digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

    def number = rep1(digit) map (l => l.mkString)

    def root: Parser[Any] = obj | arr | string | number ^^ (_.toInt) |
                             "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)

    def obj: Parser[Map[String, Any]] = "{" ~> w ~> repsep(member, w ~>","<~ w) <~ w <~"}" ^^ (Map() ++ _)

    def arr: Parser[List[Any]] = "["~> w ~> repsep(root, w ~>","<~ w) <~ w <~"]"

    def member: Parser[(String, Any)] = (string ~ w ~ ":") ~ w ~ root map {case name ~ w1 ~ ":" ~ w2 ~ value => (name, value)}
}