package pcap

import java.io.Reader
import java.lang.Long

import chunked.BoundaryReader
import pcap.ByteOrder.{ByteOrder, Reversed, Standard}

import scala.util.parsing.combinator.RegexParsers

trait PcapParser extends RegexParsers {

  def hexadecimalDigit = """[0-9a-fA-F]""".r

  def parseAll(input: Reader) = parse(pcap, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString)
  }

  def pcap: Parser[PcapFile] = pcapGlobalHeader flatMap {
    case (order, header) => rep(pcapPacket(order, header.getNetworkPacketType)) map (l => new PcapFile(header, l))
  }

  /*
  Header parsers
   */
  def pcapGlobalHeader: Parser[(ByteOrder, PcapGlobalHeader)] = magicNumber flatMap {
    case (order, magicNumber) => version(order) ~ thisZone(order) ~ sigFigs(order) ~ snapLen(order) ~ network(order) map {
      case v ~ z ~ si ~ sn ~ ne => (order, new PcapGlobalHeader(magicNumber, v._1, v._2, z, si, sn, ne))
    }
  }

  def magicNumber: Parser[(ByteOrder, Long)] = ("a1b2" ~ "c3d4") | ("d4c3" ~ "b2a1") map {
    case "a1b2" ~ "c3d4" => (Standard, Long.parseLong("a1b2c3d4", 16))
    case "d4c3" ~ "b2a1" => (Reversed, Long.parseLong("d4c3b2a1", 16))
  }

  def version(order: ByteOrder): Parser[(Int, Int)] = readTwoBytes(order) ~ readTwoBytes(order) map (t => (Integer.parseInt(t._1, 16), Integer.parseInt(t._2, 16)))

  def thisZone(order: ByteOrder): Parser[Int] = readInt(order)

  def sigFigs(order: ByteOrder): Parser[Int] = readInt(order)

  def snapLen(order: ByteOrder): Parser[Int] = readInt(order)

  def network(order: ByteOrder): Parser[Int] = readInt(order)

  /*
  Packet parsers
   */
  def pcapPacket(order: ByteOrder, networkPacketType: NetworkPacketType): Parser[PcapPacket] =
    pcapPacketHeader(order) flatMap (header => pcapPacketData(order, header.getInclLen, networkPacketType) map (data => new PcapPacket(header, data)))

  def pcapPacketHeader(order: ByteOrder): Parser[PcapPacketHeader] =
    readTimestamp(order) ~ readTimestampOffset(order) ~ readInclLen(order) ~ readOrigLen(order) map { case t1 ~ t2 ~ incl ~ orig => new PcapPacketHeader(t1, t2, incl, orig) }

  def readTimestamp(order: ByteOrder): Parser[Int] = readInt(order)

  def readTimestampOffset(order: ByteOrder): Parser[Int] = readInt(order)

  def readInclLen(order: ByteOrder): Parser[Int] = readInt(order)

  def readOrigLen(order: ByteOrder): Parser[Int] = readInt(order)

  //TODO: actually the order does not matter, as the doc states
  //def pcapPacketData(order: ByteOrder, len: Int, networkPacketType: NetworkPacketType): Parser[PcapPacketData] = repN(len, readByte) map (l => makePcapData(l.mkString, networkPacketType))

  def pcapPacketData(order: ByteOrder, len: Int, networkPacketType: NetworkPacketType): Parser[PcapPacketData] = networkPacketType match {
    case Ethernet =>  new Parser[PcapPacketData] {
      def apply(in: Input) = EthernetFrameParser.ethernetTrame(new BoundaryReader(null, in)).asInstanceOf[ParseResult[NetworkPacket]] match {
        case Success(res, rdr) => Success(new PcapPacketData(res), rdr)
        case e => throw new RuntimeException(e.toString)
      }
      //.ethernetTrame(new MutableBoundaryReader(len, null)).asInstanceOf[Parser[NetworkPacket]] map (r => new PcapPacketData(r))
    }
    case _ => throw new UnsupportedOperationException("NetworkPacket " + networkPacketType + " not supported yet")
  }


  def makePcapData(data: String, networkPacketType: NetworkPacketType): PcapPacketData = parseData(data, networkPacketType) match {
    case Success(res, _) => new PcapPacketData(res)
    case e => throw new RuntimeException(e.toString)
  }

  def parseData(data: String, networkPacketType: NetworkPacketType): ParseResult[NetworkPacket] = networkPacketType match {
    case Ethernet => EthernetFrameParser.parse(data).asInstanceOf[ParseResult[NetworkPacket]]
    case _ => throw new UnsupportedOperationException("NetworkPacket " + networkPacketType + " not supported yet")
  }

  /*
    Basic functions for reading the data
   */
  def readInt(order: ByteOrder): Parser[Int] =
    readTwoBytes(order) ~ readTwoBytes(order) map (t => Integer.parseInt(t._2 + t._1, 16))

  def readTwoBytes(order: ByteOrder): Parser[String] = readByte ~ readByte map {
    case b1 ~ b2 => if (order == ByteOrder.Reversed) b2 + b1 else b1 + b2
  }

  def readByte: Parser[String] = repN(2, hexadecimalDigit) map (_.mkString)

  def readHexadecimalDigit: Parser[String] = hexadecimalDigit
}