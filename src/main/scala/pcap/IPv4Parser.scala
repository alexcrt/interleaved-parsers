package pcap

import scala.util.parsing.combinator.RegexParsers

object IPv4Parser extends RegexParsers {

  def hexadecimalDigit = """[0-9a-fA-F]""".r
  def readHexadecimalDigit: Parser[String] = hexadecimalDigit

  def readByte: Parser[String] = repN(2, readHexadecimalDigit) map (_.mkString)


  def hexaToInt:Parser[Int] = readHexadecimalDigit map (x => Integer.parseInt(x, 16))

  //32/8 - 2 => 32 bits/line; /8 because the data is in hexa; - 2 to substract the byte we just read
  def root: Parser[IPv4Payload] = ipVersion ~ ihl flatMap (x => payload(x._1, x._2))

  def payload(ipVersion: Int, ihl: Int) : Parser[IPv4Payload] = rep(readHexadecimalDigit) map (l => new IPv4Payload(ipVersion, ihl))

  def ipVersion = hexaToInt
  def ihl = hexaToInt
}
