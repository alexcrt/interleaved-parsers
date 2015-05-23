package benchmarks

import java.io.{File, FileReader}

import chunkedV2.{JsonParser, MutableBoundaryReader, NumberParser}
import org.scalameter.{Gen, PerformanceTest}

import scala.collection.immutable.PagedSeq
import scala.io.Source
import scala.util.parsing.input.PagedSeqReader

/**
 * Created by alex on 23.05.15.
 */
object Benchmark extends PerformanceTest.Quickbenchmark {
  val maxChunkSizes = List(1000, 700, 500, 400, 300, 150, 100, 50, 10, 1)

  println("Generate data")
  for (i <- 1 until 10) {
    ChunkedGeneratorForBenchmark.generate("temp" + i, maxChunkSizes(i))
  }

  val readers =
    for (i <- 1 until 10) yield new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File("benchmark_files/temp" + i)).bufferedReader()))

  val standardFile: Gen[FileReader] = Gen.single("standard")(new FileReader(new File("benchmark_files/randomjson")))
  println("Start benchmark")

  performance of "One pass" in {
    measure method "parse" in {
      using(standardFile) in {
        rdr => JsonParser.parse(rdr)
      }
    }
  }

  //TODO: La je veux comparer par rapport a un input chunked

  /*performance of "Chunked" in {
    for (i <- 1 until 10) {
      measure method "root" in {
        val buf = new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File("benchmark_files/temp" + i)).bufferedReader()))
        val res3 = JsonParser.root(new MutableBoundaryReader(NumberParser.number, buf)).get
      }
    }
  }*/

  /*
  println("Verification of the outputs")
  for (i <- 1 until 10) {
    println("ok")
  }*/
}
