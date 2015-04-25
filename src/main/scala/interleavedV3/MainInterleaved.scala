package interleavedV3

import scala.util.parsing.input.CharSequenceReader

/**
 * Created by alex on 25.04.15.
 */
object MainInterleaved {

  def main(args: Array[String]) = {
    val res = BoundaryTextParser.parseBonjour(new MutableBoundaryReader(new CharSequenceReader("3bon4jour")))
    println(res)

    val jsonRes = JsonBoundaryParser.root(new CharSequenceReader("{\"name\":\"value\"}"))
    println(jsonRes)

    val jsonResBound = JsonBoundaryParser.root(new MutableBoundaryReader(new CharSequenceReader("2{\"4name2\":1\"7value\"}")))
    println(jsonResBound)
  }

}
