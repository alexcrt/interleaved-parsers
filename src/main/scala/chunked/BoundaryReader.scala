package chunked

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{OffsetPosition, Position, Reader}

import NumberParser.Parser
import NumberParser.Success

/**
 * Created by alex on 24.04.15.
 */

object NumberParser extends RegexParsers {

  override def skipWhitespace = false

  def CRLF = "\n"
  def number: Parser[Int] = CRLF ~>  """\d+""".r <~ CRLF map (x => x.toInt)
}

class MutableBoundaryReader private (length: Int, parseLen: Parser[Int], reader: Reader[Char]) extends Reader[Char] {

  var len = 0
  var rdr = reader
  var position = 0

  def this(parseLen: Parser[Int], reader: Reader[Char]) = this(0, parseLen, reader)

  override def first: Char = {
    if(len == 0) {
      if(atEnd) {
        throw new NoSuchElementException()
      } else {
        parseLen(rdr) match {
          case Success(le, rdrDropped) => len = le; rdr = rdrDropped
          case e => throw new RuntimeException(e.toString)
        }
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
    position = position + 1
    len = len - 1
    rdr = rdr.rest
    return this
  }
}
