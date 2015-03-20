package interleavedV2

import scala.util.parsing.input.{Reader, OffsetPosition, Position}

class ChunkReader(length: Int, rdr: Reader[Char]) extends Reader[Char] {

  override def first: Char = rdr.first

  override def atEnd: Boolean = length == 0

  override def offset = rdr.offset

  override def pos: Position = new OffsetPosition(rdr.source, rdr.offset)

  override def rest:ChunkReader = {
    if(atEnd) {
      if(first.isDigit) new ChunkReader(first.asDigit, rdr.rest)
      else throw new Exception("Wrong chunked encoding. Should terminate but character is " + first);
    }
    else new ChunkReader(length-1, rdr.rest)
  }
}

