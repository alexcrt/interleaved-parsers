package parsers

import scala.util.parsing.combinator._
import java.io.Reader

class CsvParser(input: Reader) extends JavaTokenParsers {

    def parse(): ParseResult[Any] = {
        parseAll(csvFile, input)
    }

    def csvFile: Parser[Any] = rep(csvRecord)
    def csvRecord: Parser[Any] = csvStringList~'\n'
    def csvStringList: Parser[Any] = rawString ~ opt(','~csvStringList)
    def rawString: Parser[Any] = optionalSpaces ~ opt(rawField ~ optionalSpaces)
    def optionalSpaces: Parser[Any] = whitespace*
    def whitespace: Parser[Any] = " " | '\t'
    def rawField: Parser[Any] = simpleField | quotedField 
    def simpleField: Parser[Any] = rep("""[^"\n\t\r\n\s,"]""".r)
    def quotedField: Parser[Any] = '"'~escapedField~'"'
    def escapedField: Parser[Any] = subField~ opt('"'~'"'~escapedField)
    def subField: Parser[Any] = rep("""[^"\r\n]""".r)
}