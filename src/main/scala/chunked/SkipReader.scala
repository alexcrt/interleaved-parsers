package chunked

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{OffsetPosition, Position, Reader}

import NumberParser.Parser
import NumberParser.Success

/**
 * Created by alex on 24.04.15.
 */

object ChunkListParser extends RegexParsers {

  override def skipWhitespace = false

  def parse(input: Reader[Char]) = parseAll(root, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString);
  }

  def CRLF = "\n"

  def root:Parser[List[(Int, Int)]] =
    rep(number flatMap(i => repN(i._1, acceptIf(e => true)(e => "that's not gonna happen")) map (x => (i._1, i._2))))

  def number: Parser[(Int, Int)] = CRLF ~>  """\d+""".r <~ CRLF map (x => (x.toInt, x.length + 2))
}

class SkipReader (chunks: List[(Int, Int)], reader: Reader[Char]) extends Reader[Char] {

  var len = 0
  var rdr = reader
  var position = 0

  override def first: Char = {
    if(len == 0) {
      if(atEnd) {
        throw new NoSuchElementException()
      } else {
        val chunk = chunks(position)
        position = position + 1
        len = chunk._1
        rdr = rdr.drop(chunk._2)
        return rdr.first
      }
    }
    rdr.first
  }

  override def drop(n : Int): Reader[Char] = {
    len = len - n
    if(len < 0) {
      throw new IllegalStateException("Called dropped, now the length is less than 0")
    }
    rdr = rdr.drop(n)
    return this
  }

  override def atEnd: Boolean = rdr.atEnd

  override def pos: Position = new OffsetPosition(rdr.source, position)

  override def rest: Reader[Char] = {
    len = len - 1
    rdr = rdr.rest
    return this
  }
}
