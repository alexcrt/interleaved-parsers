package interleavedV3

import scala.util.parsing.input.CharSequenceReader

/**
 * Created by alex on 25.04.15.
 */
object MainInterleaved {

  def main(args: Array[String]) = {
    val res = BoundaryTextParser.parseBonjour(new MutableBoundaryReader(new CharSequenceReader("\n3\nbon\n4\njour0")))
    println(res)

    val jsonRes = JsonBoundaryParser.root(new CharSequenceReader("{\"name\":\"value\"}"))
    println(jsonRes)

    val jsonResBound = JsonBoundaryParser.root(new MutableBoundaryReader(new CharSequenceReader("\n2\n{\"\n4\nname\n2\n\":\n1\n\"\n7\nvalue\"}\n0")))
    println(jsonResBound)
  }

}
