package benchmarks

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import java.util.stream.Collector

import chunked.DumpChunkedIntoString

import scala.io.Source
import scala.util.parsing.input.CharArrayReader
import java.util.stream.Collectors.joining

/**
 * Created by alex on 30.05.15.
 */
object HelperStats {

  def main(args: Array[String]): Unit = {
    val dirList = List(100, 1000, 10000).map(x => "benchmark_files/" + x + "_lines")
    val maxChunkSizes = List(1000, 700, 500, 300, 100, 50, 10, 5)

    dirList.map(s => new File(s+"/randomJson")).foreach(f => println(f.length() / 1024d + "KB"))
    println
    dirList.reverse
      .flatMap(d => maxChunkSizes.map(size => d+"/randomChunked"+size+"Json"))
      .map(s => (s, new File(s).length() / 1024d, DumpChunkedIntoString.parse(new CharArrayReader(Source.fromFile(s).iter.toArray)).getBytes(StandardCharsets.US_ASCII).length / 1024d))
      .foreach(t => println("File " + t._1 + "\nSize of file : " + t._2 + "KB\n" + "Size of string : " + t._3 + "KB\nSize of both : "+ (t._2 + t._3)+"KB\n=================="))

  }
}
