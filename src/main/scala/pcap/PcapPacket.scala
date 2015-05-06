package pcap

import java.time.{ZoneOffset, LocalDateTime}

class PcapPacket(header: PcapPacketHeader, data: PcapPacketData) {
  override def toString = "Header:\n"+header+"\n\nData:\n"+data
}

class PcapPacketHeader(timestamp: Int, timestampOffset: Int, inclLen: Int, origLen: Int) {
  //assume UTC for now but it's given by the thisZone field in the global header
  val date = LocalDateTime.ofEpochSecond(timestamp, timestampOffset, ZoneOffset.UTC)

  def getInclLen = inclLen

  override def toString = date.toString
}

class PcapPacketData (linkLayer: LinkLayer, ipHeader: IpHeader){

}
class LinkLayer(srcMac: String, destMac: String, ethernetType: String) {

}

abstract sealed class IpHeader
class IPv4Header extends IpHeader {

}
class IPv6Header extends IpHeader {

}