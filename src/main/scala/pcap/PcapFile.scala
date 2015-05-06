package pcap

class PcapFile(header: PcapGlobalHeader, records: List[PcapPacket]) {
  override def toString = header + "\n" + "\n==========\n"+records.mkString("\n=====\n")
}
