package parsercombinator

import java.io.FileReader
import parsers.JsonParser
import parsers.CsvParser

object ParseExpr {
    def main(args: Array[String]) {
        /*val result = Arith.parse(args(0))
        println(result)
        
        val jsonResult = JsonParser.parse(new FileReader(args(1)))
        println(jsonResult)*/
        
        val csvResult = CsvParser.parse(new FileReader(args(2)))
        println(csvResult)
    }
}