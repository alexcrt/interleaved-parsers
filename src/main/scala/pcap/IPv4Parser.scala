package pcap

import pcap.Precedence.Precedence
import pcap.Protocol.Protocol

import scala.util.parsing.combinator.RegexParsers

object IPv4Parser extends RegexParsers {

  def hexadecimalDigit = """[0-9a-fA-F]""".r

  def readHexadecimalDigit: Parser[String] = hexadecimalDigit

  def readByte: Parser[String] = repN(2, readHexadecimalDigit) map (_.mkString)

  def readTwoBytes: Parser[String] = readByte ~ readByte map { case b1 ~ b2 =>  b1 + b2 }

  def hexaToInt: Parser[Int] = readHexadecimalDigit map (x => Integer.parseInt(x, 16))

  def root: Parser[IPv4Payload] = ipVersion ~ ihl flatMap (x => payload(x._1, x._2))

  def payload(ipVersion: Int, ihl: Int): Parser[IPv4Payload] = repN(ihlLength(ihl), readHexadecimalDigit) map (l => makeIPv4Header(ipVersion, ihl, l.mkString))

  def ipVersion = hexaToInt

  def ihl = hexaToInt

  //-8 because we just read 8 bits from the field; /4 because we read the data in hexadecimal
  def ihlLength(ihl: Int) = ((32 * ihl) - 8) / 4

  def makeIPv4Header(ipVersion: Int, ihl: Int, headerRest: String): IPv4Payload = parse(headerRestParser, headerRest) match {
    case Success(res, _) =>
      new IPv4Payload(ipVersion, ihl, res._1, res._2, res._3, res._4, res._5, res._6, res._7, res._8, res._9, res._10, res._11)
    case e => throw new RuntimeException(e.toString)
  }

  def headerRestParser: Parser[(TypeOfService, Int, Int, Int, Int, Int, Protocol, Int, Ip, Ip, Option[Options])] =
    tos ~ totalLength ~ identification ~ flagsAndOffsetFrag ~ ttl ~ protocol ~ headerCheckSum ~ ip ~ ip ~ opt(options) map {
      case a ~ b ~ c ~ d ~ e ~ f ~ g ~ h ~ i ~ j => (a, b, c, d._1, d._2, e, f, g, h, i, j)
    }

  def tos: Parser[TypeOfService] = readByte map { b =>
    val s = hexaToBinary(b, 8)
    def bitSet(bit: Char) = bit == '1'
    //the two last bits are currently unused
    new TypeOfService(Precedence.fromId(Integer.parseInt(s.substring(0, 3))), bitSet(s.charAt(3)), bitSet(s.charAt(4)), bitSet(s.charAt(5)))
  }

  def totalLength: Parser[Int] = readTwoBytes map (x => Integer.parseInt(x, 16))

  def identification: Parser[Int] = readTwoBytes map (x => Integer.parseInt(x, 16))

  def flagsAndOffsetFrag: Parser[(Int, Int)] =
    readTwoBytes map (x => (Integer.parseInt(x.substring(0, 3), 16), Integer.parseInt(x.substring(3), 16)))

  def ttl: Parser[Int] = readByte map (x => Integer.parseInt(x, 16))

  def protocol: Parser[Protocol] =  readByte map (x => Protocol.fromId(Integer.parseInt(x, 16)))

  def headerCheckSum: Parser[Int] = readTwoBytes map (x => Integer.parseInt(x, 16))

  def ip: Parser[Ip] = repN(4, readByte) map (l => new Ip(l.map(i => Integer.parseInt(i, 16))))

  //TODO: options
  def options: Parser[Options] = opt(readByte) map (x => new Options())


  private def hexaToBinary(hexa: String, padding: Int): String = {
    val s = Integer.toBinaryString(Integer.parseInt(hexa, 16))
    val toPad = padding - s.length
    if(toPad > 0) {
      val sb = new StringBuilder
      for(i <- 1 to toPad) sb.append('0')
      return sb.append(s).toString()
    }
    s
  }
}

class IPv4Payload(version: Int,
                  ihl: Int,
                  tos: TypeOfService,
                  totalLength: Int,
                  identification: Int,
                  flags: Int,
                  offsetFrag: Int,
                  ttl: Int,
                  protocol: Protocol,
                  headerCheckSum: Int,
                  ipSource: Ip,
                  ipDest: Ip,
                  options: Option[Options] = None) extends Payload(Ethernet.typeMapInversed.get("IPv4").get) {

  override def toString =
    List("IP version " + version,
         "Internet Header Length: " + ihl,
         "\nType of service: \n"+tos+"\n",
         "Total length: "+totalLength,
         "Identification: "+identification,
         "Flags: "+flags,
         "OffsetFrag: "+offsetFrag,
         "TTL: "+ttl,
         "Protocol: "+protocol,
         "Header checksum: "+headerCheckSum,
         "IP source: "+ipSource,
         "IP dest: "+ipDest,
         "Options: "+options).mkString("\n")
}

class TypeOfService(precedence: Precedence, delay: Boolean, throughput: Boolean, reliability: Boolean) {

  def normalOrLow(b: Boolean) = if(b) "Low" else "Normal"
  def normalOrHigh(b: Boolean) = if(b) "Normal" else "High"

  override def toString = List("Precedence: " + precedence, "Delay: " + normalOrLow(delay), "Throughput: " + normalOrHigh(throughput), "Reliability: " + normalOrHigh(reliability)).mkString("\n")
}

object Precedence extends Enumeration {
  type Precedence = Value
  val Routine = Value(0)
  val Priority = Value(1)
  val Immediate = Value(2)
  val Flash = Value(3)
  val Flash_Override = Value(4)
  val Critic_Ecp = Value(5)
  val Internetwork_Control = Value(6)
  val Network_Control = Value(7)

  def fromId(id: Int): Precedence = Precedence.values.find(p => p.id == id).get
}

object Protocol extends Enumeration {
  type Protocol = Value
  val ICMP = Value(1)
  val TCP = Value(6)
  val UDP = Value(17)

  def fromId(id: Int): Protocol = Protocol.values.find(p => p.id == id).get
}

class Ip (list: List[Int]) {
  override def toString = list.mkString(":")
}

class Options