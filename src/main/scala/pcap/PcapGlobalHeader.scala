package pcap

import java.lang.Long

class PcapGlobalHeader(magicNumber: Long, versionMajor: Int, versionMinor: Int, thisZone: Int, sigFigs: Int, snapLen: Int, network: Int) {

  def getNetworkPacketType = NetworkPacketType.typeMap.getOrElse(network, throw new NoSuchElementException())

  override def toString = "PcapHeader: \n" +
    "Magic number: " + Long.toHexString(magicNumber) + "\n" +
    "Version: " + versionMajor + "." + versionMinor + "\n" +
    "thisZone: " + thisZone + " - " + Integer.toHexString(thisZone) + "\n" +
    "sigFigs: " + sigFigs + " - " + Integer.toHexString(sigFigs) + "\n" +
    "snapLen: " + snapLen + " - " + Integer.toHexString(snapLen) + "\n" +
    "network: " + network + " - " + Integer.toHexString(network)
}