package pcap

import scala.util.parsing.combinator.RegexParsers

object IPv4Parser extends RegexParsers {

  //TODO: debug this strange behavior with the input / follow the execution flow
  var i = 0
  val list = List(26, 67)
  def next = {val u = list(i); i = (i + 1)%2; println(u); u}

  def hexadecimalDigit = """[0-9a-fA-F]""".r
  def readHexadecimalDigit: Parser[String] = hexadecimalDigit

  def readByte: Parser[String] = repN(2, readHexadecimalDigit) map (_.mkString)


  def hexaToInt:Parser[Int] = readHexadecimalDigit map (x => Integer.parseInt(x, 16))

  //32/8 - 2 => 32 bits/line; /8 because the data is in hexa; - 2 to substract the byte we just read
  //TODO: remove + 26 * 4
  def root: Parser[IPv4Payload] = ipVersion ~ ihl flatMap {
    x => repN(32/8 * x._1 - 2 + next*4, readHexadecimalDigit) map (l => new IPv4Payload(x._1, x._2))
  }

  def ipVersion = hexaToInt
  def ihl = hexaToInt
}
