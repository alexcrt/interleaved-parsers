package interleaved


import scala.util.{Failure, Success}
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, CharSequenceReader, Reader}


object Interleaved extends RegexParsers {

  def digit: Parser[Int] = """\d+""".r map (x => x.toInt)

  def digitAndContent: Parser[ChunkReader] = digit flatMap (x => content(x))

  def content(length: Int): Parser[ChunkReader] = Parser { self =>
    Success(new ChunkReader(length, self), self.drop(length))
  }

  def wikiParseReader: Parser[List[ChunkReader]] = rep(digitAndContent)

  def main(args: Array[String]) = {
    println(wikiParseReader(new CharSequenceReader("3Sca2la")))
    println(wikiParseReader(new CharSequenceReader("3Chu2nk3ed 1T4rans3fer2 i3s c3ool1!")))
  }
}

class ChunkReader(length: Int, rdr: Reader[Char]) {
  def content = rdr.source.subSequence(rdr.offset, rdr.offset + length)
  override def toString = length+"-"+content
}


