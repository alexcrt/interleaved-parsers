package pcap

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by alex on 10.05.15.
 */
object EthernetFrameParser extends RegexParsers {

  def hexadecimalDigit = """[0-9a-fA-F]""".r

  def parse(input: String): ParseResult[NetworkPacket] = parse(ethernetTrame, input)

  def ethernetTrame: Parser[NetworkPacket] =
    macHeader flatMap (h => payload(h.getEtherType) map (d => new EthernetFrame(h, d)))

  def macHeader: Parser[MACHeader] =
    macAddress ~ macAddress ~ etherType map {case src ~ dest ~ ethType => new MACHeader(src, dest, Integer.parseInt(ethType, 16))}


  //TODO: real mac address parsing
  def macAddress: Parser[String] = repN(3, repN(4, hexadecimalDigit)) map (l => l.map(_.mkString).mkString(":"))

  //TODO: return ethernet type
  def etherType: Parser[String] = repN(4, hexadecimalDigit) map (_.mkString)

  def payload(etherType: String): Parser[Payload] = etherType match {
    case "IPv4" => IPv4Parser.root.asInstanceOf[Parser[Payload]]
    case _ => throw new UnsupportedOperationException(etherType+ "not supported yet")
  }
}
