package interleaved


import java.io.{File, FileReader}
import java.nio.file.Paths.get
import java.nio.file.Files.lines
import java.util.stream.Collectors.toList

import scala.util.{Failure, Success}
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, CharSequenceReader, Reader}

import scala.collection.JavaConversions._


object Chunked extends RegexParsers {

  def digit: Parser[Int] = """\d+""".r map (x => x.toInt)

  def digitAndContent: Parser[ChunkReader] = digit flatMap (x => content(x))

  def content(length: Int): Parser[ChunkReader] = Parser { self =>
    Success(new ChunkReader(length, self), self.drop(length))
  }

  def parseChunked: Parser[String] = rep(digitAndContent) map (x => x.foldLeft(new StringBuilder())((sb, chunk) => sb.append(chunk.content)).toString)

  def main(args: Array[String]) = {
    lines(get("testing_files/chunked")).collect(toList())
      .toList
      .foreach(text => println(parseChunked(new CharSequenceReader(text))))
  }
}

class ChunkReader(length: Int, rdr: Reader[Char]) {
  def content = rdr.source.subSequence(rdr.offset, rdr.offset + length)
  override def toString = length+"-"+content
}


