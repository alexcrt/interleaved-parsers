package benchmarks

import java.io.{FileWriter, File, FileReader}

import chunked.{BoundaryReader, JsonParser, NumberParser}

import scala.collection.immutable.PagedSeq
import scala.io.Source
import scala.util.parsing.input.PagedSeqReader

/**
 * Created by alex on 24.05.15.
 */
object UglyBenchmark {

  val dirList = List(100 , 1000, 10000, 100000).map(x => "benchmark_files/" + x + "_lines")
  val maxChunkSizes = List(1000, 700, 500, 400, 300, 150, 100, 50, 10, 1)

  val warmupIterations = 50
  val realIterations = 10

  val writer = new FileWriter(new File("benchmark_results.txt"))

  def main(args: Array[String]) = {

    println("Generate Data...")

    for (dirName <- dirList) {
      for (chunkSize <- maxChunkSizes) {
        ChunkedGeneratorForBenchmark.generate(dirName + "/randomJson", dirName + "/randomChunked" + chunkSize + "Json", chunkSize)
      }
    }

    println("Verifying outputs...")
    for (dirName <- dirList) {
      println("==================")
      println(dirName)
      val res = JsonParser.parse(new FileReader(new File(dirName + "/randomJson")))
      for (chunkSize <- maxChunkSizes) {
        val resChunked =
          JsonParser.root(new BoundaryReader(NumberParser.number, new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))))
        println("Asserting result correct for chunk of max size " + chunkSize)
        assert(res.equals(resChunked.get))
      }
    }

    println("Mesuring time...")
    writer.write("Standard vs Chunked\n")
    for (dirName <- dirList) {
      writer.write(dirName+"\n")
      writer.write("Warmup... Doing "+warmupIterations+" iterations...\n")
      for (i <- 0 until warmupIterations) {
        for (chunkSize <- maxChunkSizes) {
          val res = JsonParser.parse(new FileReader(new File(dirName + "/randomJson")))
          val resChunked =
            JsonParser.root(new BoundaryReader(NumberParser.number, new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))))

          //Not sure if needed but it's for not letting JIT kick the results since we don't use them otherwise
          println(res.equals(resChunked.get))
        }
      }
      writer.write("Testing\n")
      for (chunkSize <- maxChunkSizes) {
        writer.write("Standard vs Chunk of max size "+chunkSize+"\n")
        for (i <- 0 until realIterations) {
          writer.write("Iteration " + (i + 1) + " =>\n")
          //Standard
          val rdr = new FileReader(new File(dirName + "/randomJson"))
          var startTime = System.nanoTime()
          val res = JsonParser.parse(rdr)
          var endTime = System.nanoTime() - startTime
          writer.write("\tStandard: " + endTime + " nanoseconds\n")

          //Chunked
          val buf = new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))
          startTime = System.nanoTime()
          val resChunked = JsonParser.root(new BoundaryReader(NumberParser.number, buf))
          endTime = System.nanoTime() - startTime
          writer.write("\tChunked: " + endTime + " nanoseconds\n\n")

          //Not sure if needed but it's for not letting JIT kick the results since we don't use them otherwise
          println(res.equals(resChunked.get))
        }
      }

      writer.close()
    }


  }
}