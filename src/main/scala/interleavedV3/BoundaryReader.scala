package interleavedV3

import scala.util.parsing.input.{OffsetPosition, Position, Reader}

/**
 * Created by alex on 24.04.15.
 */
abstract class BoundaryReader(length: Int, reader: Reader[Char]) extends Reader[Char]

object NumberParser extends MyRegexParsers {

  def CRLF = "\n" | "\r\n"

  def digit: Parser[String] = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
  def number: Parser[(Int, Int)] = CRLF ~> rep1(digit) <~ CRLF map (x => (x.length + 2, x.mkString.toInt))

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
        val t = NumberParser.number(rdr).get
        rdr = rdr.drop(t._1)
        len = t._2
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
