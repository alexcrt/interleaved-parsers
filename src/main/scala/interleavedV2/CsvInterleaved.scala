package interleavedV2

import java.io.{BufferedReader, InputStreamReader, InputStream, StringReader}
import java.nio.file.Files.lines
import java.nio.file.Paths.get
import java.util.stream.Collectors.toList

import MimeParsing.CsvParser

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{PagedSeqReader, CharSequenceReader, Reader}


object CsvInterleaved {

  def main(args: Array[String]) = {

    val file = Source.fromFile("testing_files/chunkedCSV").mkString
    var reader = new ChunkReader(0, new CharSequenceReader(file))
    while(reader.offset != file.length) {
      print(reader.first + " => ")
      reader = reader.rest
    }
    //val result = CsvParser.parse(reader);
  }
}


