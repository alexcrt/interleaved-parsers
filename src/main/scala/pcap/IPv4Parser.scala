package pcap

import scala.util.parsing.combinator.RegexParsers

object IPv4Parser extends RegexParsers {

  def re = PcapP
  def root(data: String): Parser[IPv4Payload] = "" ^^ (x => new IPv4Payload(data))
}
