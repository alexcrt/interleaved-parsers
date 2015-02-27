package main

import java.io.FileReader
import parsers.JsonParser
import parsers.CsvParser
import parsers.BnfParser

object ParseExpr {
    def main(args: Array[String]) {
      /*val csvResult = CsvParser.parse(new FileReader(args(0)))
      println(csvResult)

      val jsonResult = JsonParser.parse(new FileReader(args(1)))
      println(jsonResult)*/
        
      val bnfResult = BnfParser.parse(new FileReader(args(2)))
      println(bnfResult)
    }
}