package pcap

import java.io.{BufferedReader, ByteArrayOutputStream}
import java.nio.ByteBuffer
import javax.sql.rowset.serial.SerialBlob

import scala.io.Source

object PcapMain {

  def main(args: Array[String]) {
    val parser = new PcapParser{}
    val file = Source.fromFile("testing_files/pcap/pcapFile.pcap")
    val res = parser.parseAll(file.bufferedReader())
    println(res)
  }
}
