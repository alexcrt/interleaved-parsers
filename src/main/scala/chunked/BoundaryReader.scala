package chunked

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{OffsetPosition, Position, Reader}

/**
 * Created by alex on 24.04.15.
 */
abstract class BoundaryReader(length: Int, reader: Reader[Char]) extends Reader[Char]

object NumberParser extends RegexParsers {

  override def skipWhitespace = false

  def CRLF = "\n"
  def number: Parser[Int] = CRLF ~>  """\d+""".r <~ CRLF map (x => x.toInt)

  def root(reader: Reader[Char]) = parse(number, reader) match {
    case Success(res, rdr) => (res, rdr)
    case e => throw new RuntimeException(e.toString)
  }
}

case class MutableBoundaryReader(length: Int, reader: Reader[Char]) extends BoundaryReader (length, reader) {

  var len = length
  var rdr = reader
  var position = 0

  def this(reader: Reader[Char]) = this(0, reader)

  override def first: Char = {
    if(len == 0) {
      if(atEnd) {
        throw new NoSuchElementException()
      } else {
        val t = NumberParser.root(rdr)
        len = t._1
        rdr = t._2
        return rdr.first
      }
    }
    rdr.first
  }

  override def drop(n : Int): Reader[Char] = {
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
