package chunkedV2

import java.io.{File, FileReader}
import java.nio.file.{Paths, Files}
import java.util.stream.Collectors

import scala.util.parsing.input.{PagedSeqReader, CharSequenceReader}

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
    val res2 = JsonBoundaryParser.parse(new FileReader(new File("testing_files/demoJSON")))
    println(res2)

    val content = "\n7\n{  \"pro\n13\nduct\": \"Live \n8\nJSON\",  \n13\n\"demo\": true,\n16\n  \"person\": {   \n7\n \"id\": \n4\n1234\n4\n5,  \n6\n  \"nam\n1\ne\n2\n\":\n10\n \"John Doe\n13\n\",    \"phones\n3\n\": \n10\n{      \"ho\n19\nme\": \"800-123-4567\"\n17\n,      \"mobile\": \n2\n\"8\n2\n77\n3\n-12\n13\n3-1234\"    },\n18\n    \"email\": [    \n2\n  \n3\n\"jd\n18\n@example.com\",    \n13\n  \"jd@example\n16\n.org\"    ],    \"\n17\nregistered\": true\n18\n,    \"emergencyCon\n10\ntacts\": [ \n18\n     {        \"nam\n17\ne\": \"Jane Doe\",  \n16\n      \"phone\": \"\n3\n888\n19\n-555-1212\",        \n13\n\"relationship\n6\n\": \"sp\n1\no\n3\nuse\n14\n\"      },     \n10\n {        \n2\n\"n\n19\name\": \"Justin Doe\",\n11\n        \"ph\n14\none\": \"877-123\n2\n-1\n7\n212\",  \n12\n      \"relat\n1\ni\n19\nonship\": \"parent\"  \n14\n    }    ]  }}"
    val reader = new CharSequenceReader(content)
    val res3 = JsonBoundaryParser.root(new MutableBoundaryReader(NumberParser.number, reader)).get
    println(res3)

    println(res2.equals(res3))

  }

}
