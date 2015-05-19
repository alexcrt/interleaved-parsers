package pcap

import java.io.{BufferedReader, ByteArrayOutputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import javax.sql.rowset.serial.SerialBlob

import scala.io.Source

object PcapMain {

  def main(args: Array[String]) {
    val parser = new PcapParser{}
    val file = Source.fromFile("testing_files/pcap/pcapFile.pcap")
    val res = parser.parseAll(file.bufferedReader())
    println(res)
    Files.write(Paths.get("result.txt"), res.toString.getBytes(StandardCharsets.UTF_8))
  }
}
