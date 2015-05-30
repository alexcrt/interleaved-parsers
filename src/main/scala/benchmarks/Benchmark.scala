package benchmarks

import java.io.{File, FileReader}

import chunked.{DumpChunkedIntoString, JsonParser, BoundaryReader, NumberParser}
import org.scalameter.{Gen, PerformanceTest}
import utils.JsonParserWithRegex

import scala.collection.immutable.PagedSeq
import scala.io.Source
import scala.util.parsing.input.{CharArrayReader, CharSequenceReader, PagedSeqReader}

/**
 * Created by alex on 23.05.15.
 */
object Benchmark extends PerformanceTest.Quickbenchmark {

  val dirList = List(100, 1000, 10000).map(x => "benchmark_files/" + x + "_lines")
  val maxChunkSizes = List(
    1000,
    700,
    500,
    300,
    100,
    50,
    10,
    5
  )


  val genListNonChunked = dirList.map(d => (d, Gen.single("arr")(Source.fromFile(d+"/randomjson").iter.toArray)))

  genListNonChunked.foreach(t => {
    performance of "Non-chunked input StringParsers with "+ t._1 in {
      measure method "parse" in {
        using(t._2) in {
          gen => JsonParser.parse(new CharArrayReader(gen))
        }
      }
    }

    performance of "Non-chunked input RegexParsers with "+t._1 in {
      measure method "parse" in {
        using(t._2) in {
          gen => JsonParserWithRegex.parse(new CharArrayReader(gen))
        }
      }
    }
  })

  val genListChunked = maxChunkSizes.reverse.flatMap(size => dirList.map(d => (d+"/randomChunked"+size+"Json", "chunk of size "+size, Gen.single("arr")(Source.fromFile(d+"/randomChunked"+size+"Json").iter.toArray))))

  genListChunked.foreach(t => {
    performance of "Chunked input with " + t._1 + "=> " + t._2 in {
      measure method "parse" in {
        using(t._3) in {
          gen => JsonParser.parse(new BoundaryReader(NumberParser.number, new CharArrayReader(gen)))
        }
      }
    }
  })

}
