package interleaved


import java.io.{StringReader, File, FileReader}
import java.nio.file.Paths.get
import java.nio.file.Files.lines
import java.util.stream.Collectors.toList

import parsers.CsvParser

import scala.util.{Failure, Success}
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, CharSequenceReader, Reader}

import scala.collection.JavaConversions._


object CsvInterleaved extends RegexParsers {

  def parse(in: Reader[Char]) = Chunked.parseChunked(in) map (x => CsvParser.parse(new StringReader(x)))


  def main(args: Array[String]) = {
    lines(get("testing_files/chunkedCSV")).collect(toList())
      .toList
      .foreach(text => println(parse(new CharSequenceReader(text))))
  }
}


