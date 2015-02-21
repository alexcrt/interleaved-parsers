package parsers

import scala.util.parsing.combinator._
import java.io.Reader

class CsvParser(input: Reader) extends JavaTokenParsers {

    def parse(): ParseResult[Any] = {
        parseAll(csvFile, input)
    }

    def csvFile: Parser[Any] = hdr~opt(row)
    def hdr: Parser[Any] = row
    def row: Parser[Any] = repsep(field, ',') ~ """[\r?\n"]+""".r
    def field: Parser[Any] = text | stringLiteral | ";"
    def text: Parser[Any] = """~[,\n\r]+""".r
}