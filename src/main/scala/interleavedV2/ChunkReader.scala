package interleavedV2

import java.io.StringReader

import scala.util.parsing.input.{CharSequenceReader, Reader, OffsetPosition, Position}

class ChunkReader(length: Int, rdr: Reader[Char]) extends Reader[Char] {

  def content = rdr.source.subSequence(rdr.offset, rdr.offset + length)

  override def toString = length+"-"+content

  override def first: Char = rdr.first

  override def atEnd: Boolean = length == 0

  override def pos: Position = new OffsetPosition(rdr.source, rdr.offset)

  override def rest: Reader[Char] = {
    if(atEnd) {
      if(first.isDigit) {
        return new CharSequenceReader(rdr.source, offset)
      }
      else rest
    }
    else new ChunkReader(length-1, rdr.rest)
  }
}

