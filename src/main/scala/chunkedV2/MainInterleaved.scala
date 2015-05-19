package chunkedV2

import java.io.{File, FileReader}
import java.nio.file.{Paths, Files}
import java.util.stream.Collectors

import scala.io.Source
import scala.util.parsing.input.{CharArrayReader, PagedSeqReader, CharSequenceReader}

/**
 * Created by alex on 25.04.15.
 */
object MainInterleaved {

  def main(args: Array[String]) = {
    /*
    val res = BoundaryTextParser.parseBonjour(new MutableBoundaryReader(NumberParser.number, new CharSequenceReader("\n3\nbon\n4\njour")))
    println(res)


    val jsonRes = JsonBoundaryParser.root(new CharSequenceReader("{\"a\":\"b\"}"))
    println(jsonRes)

    //println("\n2\n{\"\n4\nname\n2\n\":\n1\n\"\n8\nvalue\" }\n0")
    val toParse = "\n5\n{\n  \"\n22\nproduct\": \"Live JSON\"}"
    val jsonResBound = JsonBoundaryParser.root(new MutableBoundaryReader(NumberParser.number, new CharSequenceReader(toParse)))
    println(jsonResBound)
*/
    val res2 = JsonBoundaryParser.parse(new FileReader(new File("testing_files/demoJSON2")))
    println(res2)

    val buf = new CharArrayReader(Source.fromFile(new File("testing_files/generatedChunkedJSON")).toArray)

    val res3 = JsonBoundaryParser.root(new MutableBoundaryReader(NumberParser.number, buf)).get
    println(res3)

    println(res2.equals(res3))

  }

}
