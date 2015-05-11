package pcap

import java.io.Reader
import scala.util.parsing.combinator.RegexParsers
import java.lang.Long
import ByteOrder.ByteOrder
import ByteOrder.Standard
import ByteOrder.Reversed

trait PcapParser extends RegexParsers {

  def hexadecimalDigit = """[0-9a-fA-F]""".r

  def parseAll(input: Reader) = parse(pcap, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString)
  }

  def pcap: Parser[PcapFile] = pcapGlobalHeader flatMap {
    case (order, header) => repN(2, pcapPacket(order, header.getNetworkPacketType)) map (l => new PcapFile(header, l))
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
    pcapPacketHeader(order) flatMap (header => pcapPacketData(order, header.getInclLen, networkPacketType) map(data => new PcapPacket(header, data)))

  def pcapPacketHeader(order: ByteOrder): Parser[PcapPacketHeader] =
    readTimestamp(order) ~ readTimestampOffset(order) ~ readInclLen(order) ~ readOrigLen(order) map {case t1 ~ t2 ~ incl ~ orig => new PcapPacketHeader(t1, t2, incl, orig)}

  def readTimestamp(order: ByteOrder): Parser[Int] = readInt(order) map (x => {println(x); x})

  def readTimestampOffset(order: ByteOrder) : Parser[Int] = readInt(order)

  def readInclLen(order: ByteOrder) : Parser[Int] = readInt(order)

  def readOrigLen(order: ByteOrder) : Parser[Int] = readInt(order)

  //TODO: actually the order does not matter, as the doc states
  def pcapPacketData(order: ByteOrder, len: Int, networkPacketType: NetworkPacketType): Parser[PcapPacketData] = repN(len, readByte) flatMap (l => makePcapData(l.mkString, networkPacketType))

  def makePcapData(data: String, networkPacketType: NetworkPacketType): Parser[PcapPacketData] = networkPacketType match {
    case Ethernet => ethernetFrameParser(data) map (trame => new PcapPacketData(trame))
      //"" ^^ (x => new PcapPacketData(new EthernetFrame(null, null)))
    //ethernetTrame(data) map (trame => new PcapPacketData(trame))
    case _ => throw new UnsupportedOperationException("NetworkPacket "+networkPacketType+" not supported yet")
  }

  //hack, actually ignore input but parse the String data
  def ethernetFrameParser(data: String): Parser[NetworkPacket] = new Parser[NetworkPacket] {
    def apply(in: Input) = EthernetFrameParser.parse(data).asInstanceOf[ParseResult[NetworkPacket]]
  }

  /*
    Basic functions for reading the data
   */
  def readInt(order: ByteOrder): Parser[Int] =
    readTwoBytes(order) ~ readTwoBytes(order) map (t => Integer.parseInt(t._2+t._1, 16))

  def readTwoBytes(order: ByteOrder): Parser[String] = readByte ~ readByte map {
    case b1 ~ b2 => if(order == ByteOrder.Reversed) b2+b1 else b1+b2
  }
  def readByte: Parser[String] = repN(2, hexadecimalDigit) map (_.mkString)
  def readHexadecimalDigit: Parser[String] = hexadecimalDigit
}