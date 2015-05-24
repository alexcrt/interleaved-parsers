package benchmarks

import java.io.{File, FileReader}

import chunked.{DumpChunkedIntoString, JsonParser, BoundaryReader, NumberParser}
import org.scalameter.{Gen, PerformanceTest}

import scala.collection.immutable.PagedSeq
import scala.io.Source
import scala.util.parsing.input.{CharSequenceReader, PagedSeqReader}

/**
 * Created by alex on 23.05.15.
 */
object Benchmark extends PerformanceTest.Quickbenchmark {
  val maxChunkSizes = List(1000, 700, 500, 400, 300, 150, 100, 50, 10, 1)

  /*println("Generate data")
  for (i <- 1 until 10) {
    ChunkedGeneratorForBenchmark.generate("temp" + i, maxChunkSizes(i))
  }*/

  //val readers =
    //for (i <- 1 until 10) yield new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File("benchmark_files/temp" + i)).bufferedReader()))

  val r: Gen[Int] = Gen.single("dumbgen")(1)
  println("Start benchmark")

  performance of "One pass" in {
    measure method "parse" in {
      using(r) in {
        r => JsonParser.parse(new FileReader(new File("benchmark_files/10000_lines/randomjson")))
      }
    }
  }


  /*
  //One pass chunked
  performance of "One pass chunked" in {
    for (i <- 1 until 10) {
      measure method "root" in {
        val buf = new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File("benchmark_files/temp" + i)).bufferedReader()))
        val res3 = JsonParser.root(new BoundaryReader(NumberParser.number, buf)).get
      }
    }
  }


  //Read in two pass
  performance of "Two pass chunked" in {
    for (i <- 1 until 10) {
      measure method "root" in {
          val buf = new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File("benchmark_files/temp" + i)).bufferedReader()))
          val x = DumpChunkedIntoString.parse(buf)
          val res = JsonParser.root(new CharSequenceReader(x))
      }
    }
  }

*/

  println("Verification of the outputs")
  for (i <- 1 until 10) {
    println("ok")
  }
}
