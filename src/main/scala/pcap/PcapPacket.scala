package pcap

import java.time.{ZoneOffset, LocalDateTime}

class PcapPacket(header: PcapPacketHeader, data: PcapPacketData) {
  override def toString = "Header:\n"+header+"\n\nData:\n"+data
}

class PcapPacketHeader(timestamp: Int, timestampOffset: Int, inclLen: Int, origLen: Int) {
  //TODO: assume UTC for now but it's given by the thisZone field in the global header
  val date = LocalDateTime.ofEpochSecond(timestamp, timestampOffset, ZoneOffset.UTC)

  def getInclLen = inclLen

  override def toString = "Date of the packet: "+date+"\n"+"Number of bytes to read: "+inclLen
}

class PcapPacketData (networkPacket: NetworkPacket){
  override def toString = networkPacket.toString
}

object NetworkPacketType {
  val typeMap = Map(1 -> Ethernet)
}
sealed abstract class NetworkPacketType
object Ethernet extends NetworkPacketType {
  val typeMap = Map(
    0x0800 -> "IPv4",
    0x86DD -> "IPv6",
    0x0806 -> "ARP",
    0x8035 -> "RARP",
    0x809B -> "AppleTalk",
    0x88CD -> "SERCOS III",
    0x0600 -> "XNS",
    0x8100 -> "VLAN")

  val typeMapInversed = typeMap.map(_.swap)
}

sealed abstract class NetworkPacket

case class EthernetFrame(header: MACHeader, payload: Payload) extends NetworkPacket {
  override def toString = header + "\nPayload: \n"+payload
}

class MACHeader(destAddr: String, srcAddr: String, etherType: Int) {

  val stringEtherType = Ethernet.typeMap.getOrElse(etherType, throw new NoSuchElementException())

  def getEtherType = stringEtherType

  override def toString = "Dest MAC address: " + destAddr +
    "\nSrc MAC address: " + srcAddr +
    "\nEther type: 0x" + Integer.toHexString(etherType) + " - " + stringEtherType
}

sealed abstract class Payload (typ: Int)
class IPv4Payload(version: Int, ihl: Int) extends Payload(Ethernet.typeMapInversed.get("IPv4").get) {
  override def toString = List("Version "+version, "Internet Header Length "+ihl).mkString("\n")
}
