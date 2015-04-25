package interleavedV3

import scala.util.parsing.input.{OffsetPosition, Position, Reader}

/**
 * Created by alex on 24.04.15.
 */
abstract class BoundaryReader(length: Int, reader: Reader[Char]) extends Reader[Char]

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
        len = rdr.first.asDigit
        rdr = rdr.drop(1)
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
