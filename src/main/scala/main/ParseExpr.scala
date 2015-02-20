package parsercombinator

import java.io.FileReader;
import parsers.JsonParser

object ParseExpr {
    def main(args: Array[String]) {
        val result = new Arith(args(0)).parse()
        println(result)
        
        val jsonResult = new JsonParser(new FileReader(args(1))).parse()
        println(jsonResult)
    }
}