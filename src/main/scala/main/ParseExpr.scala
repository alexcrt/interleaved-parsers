package parsercombinator

import java.io.FileReader
import parsers.JsonParser
import parsers.CsvParser

object ParseExpr {
    def main(args: Array[String]) {
        /*val result = new Arith(args(0)).parse()
        println(result)
        
        val jsonResult = new JsonParser(new FileReader(args(1))).parse()
        println(jsonResult)*/
        
        val csvResult = new CsvParser(new FileReader(args(2))).parse()
        println(csvResult)
    }
}