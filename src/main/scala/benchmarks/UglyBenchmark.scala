package benchmarks

import java.io.{File, FileReader, FileWriter}
import java.nio.file.{Paths, Files}

import chunked.{NumberParser, DumpChunkedIntoString, BoundaryReader, JsonParser}
import utils.JsonParserWithRegex

import scala.collection.immutable.PagedSeq
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.parsing.input.{CharArrayReader, CharSequenceReader, PagedSeqReader}

/**
 * Created by alex on 24.05.15.
 */
object UglyBenchmark {

  val dirList = List(100, 1000, 10000).map(x => "benchmark_files/" + x + "_lines")
  val maxChunkSizes = List(
    1000,
    700,
    300,
    100,
    10
  )

  val warmupIterations = 50
  val realIterations = 10

  val writeIterations = false

  val writer = new FileWriter(new File("benchmark_results.txt"))

  def main(args: Array[String]) = {

    println("Generate Data...")

    for (dirName <- dirList) {
      for (chunkSize <- maxChunkSizes) {
        ChunkedGeneratorForBenchmark.generate(dirName + "/randomJson.json", dirName + "/randomChunked" + chunkSize + "Json", chunkSize, true)
      }
    }

    println("Mesuring time...")
    //stringVsRegex()
    //standardVsChunked()
    //chunkedOnePassVsChunked2pass()

    writer.close()
  }

  def stringVsRegex() = {
    println("==========================================")
    println("String vs Regex")

    writer.write("==========================================\n")
    writer.write("String vs Regex\n")


    for (dirName <- dirList) {
      println(dirName)

      val array = Source.fromFile(dirName + "/randomJson.json").iter.toArray

      writer.write("------------------------------------\n")
      writer.write(dirName + "\n")
      writer.write("------------------------------------\n")
      writer.write("Warmup... Doing " + warmupIterations + " iterations...\n")
      for (i <- 0 until warmupIterations) {
        val resRegex = JsonParserWithRegex.parse(new CharArrayReader(array))
        val resString = JsonParser.parse(new CharArrayReader(array))

        //Not letting JIT kick the results since we don't use them otherwise, and assert a correct result
        assert(resRegex.equals(resString))
      }
      writer.write("Testing... Doing " + realIterations+"...\n\n")
      var timesRegex = new ListBuffer[Long]()
      var timesString = new ListBuffer[Long]()
      for (i <- 0 until realIterations) {
        //Regex
        var startTime = System.nanoTime()
        val resRegex = JsonParserWithRegex.parse(new CharArrayReader(array))
        val endTimeRegex = System.nanoTime() - startTime
        timesRegex += endTimeRegex

        //String
        startTime = System.nanoTime()
        val resString = JsonParser.parse(new CharArrayReader(array))
        val endTimeString = System.nanoTime() - startTime
        timesString += endTimeString

        if (writeIterations) {
          writer.write("Iteration " + (i + 1) + " =>\n")
          writer.write("\tRegex: " + endTimeRegex + " nanoseconds\n")
          writer.write("\tString:  " + endTimeString + " nanoseconds\n\n")
        }

        //Not letting JIT kick the results since we don't use them otherwise, and assert a correct result
        assert(resRegex.equals(resString))
      }


      assert(timesRegex.size == timesString.size)

      var min = timesRegex.min
      var max = timesRegex.max
      var mean = timesRegex.sum / timesRegex.size

      writer.write("\tRegex min time: " + min + " nanoseconds / " + min / math.pow(10, 6) + " milliseconds / " + min / math.pow(10, 9) + " seconds \n")
      writer.write("\tRegex max time: " + max + " nanoseconds / " + max / math.pow(10, 6) + " milliseconds / " + max / math.pow(10, 9) + " seconds \n")
      writer.write("\tRegex mean time: " + mean + " nanoseconds / " + mean / math.pow(10, 6) + " milliseconds / " + mean / math.pow(10, 9) + " seconds \n")
      writer.write("\n")

      min = timesString.min
      max = timesString.max
      mean = timesString.sum / timesString.size

      writer.write("\tString min time: " + min + " nanoseconds / " + min / math.pow(10, 6) + " milliseconds / " + min / math.pow(10, 9) + " seconds \n")
      writer.write("\tString max time: " + max + " nanoseconds / " + max / math.pow(10, 6) + " milliseconds / " + max / math.pow(10, 9) + " seconds \n")
      writer.write("\tString mean time: " + mean + " nanoseconds / " + mean / math.pow(10, 6) + " milliseconds / " + mean / math.pow(10, 9) + " seconds \n")

    }
    writer.write("==========================================")
  }

  def standardVsChunked() = {
    println("==========================================")
    println("Standard vs Chunked")
    writer.write("==========================================\n")
    writer.write("Standard vs Chunked\n")

    for (dirName <- dirList) {
      println(dirName)
      writer.write("------------------------------------\n")
      writer.write(dirName + "\n")
      writer.write("------------------------------------\n")
      for (chunkSize <- maxChunkSizes) {
        writer.write("Warmup... Doing " + warmupIterations + " iterations on max chunk size "+chunkSize+" ...\n")
        for (i <- 0 until warmupIterations) {
          val res = JsonParser.parse(new FileReader(new File(dirName + "/randomJson.json.json.json")))
          val resChunked =
            JsonParser.root(new BoundaryReader(NumberParser.number, new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))))

          //Not letting JIT kick the results since we don't use them otherwise, and assert a correct result
          if (!res.equals(resChunked.get)) {
            println("not equals")
            throw new AssertionError()
          }
        }
        var timesStandard = new ListBuffer[Long]()
        var timesChunked = new ListBuffer[Long]()

        println("Standard vs Chunk of max size " + chunkSize)

        writer.write("Testing... Doing " + realIterations+"...\n\n")
        writer.write("Standard vs Chunk of max size " + chunkSize + "\n\n")
        for (i <- 0 until realIterations) {
          //Standard
          val rdr = new FileReader(new File(dirName + "/randomJson.json"))
          var startTime = System.nanoTime()
          val res = JsonParser.parse(rdr)
          val endTimeStandard = System.nanoTime() - startTime
          timesStandard += endTimeStandard

          //Chunked
          val buf = new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))
          startTime = System.nanoTime()
          val resChunked = JsonParser.root(new BoundaryReader(NumberParser.number, buf))
          val endTimeChunked = System.nanoTime() - startTime
          timesChunked += endTimeChunked

          if (writeIterations) {
            writer.write("Iteration " + (i + 1) + " =>\n")
            writer.write("\tStandard: " + endTimeStandard + " nanoseconds\n")
            writer.write("\tChunked:  " + endTimeChunked + " nanoseconds\n\n")
          }

          //Not letting JIT kick the results since we don't use them otherwise, and assert a correct result
          if (!res.equals(resChunked.get)) {
            println("not equals")
            throw new AssertionError()
          }
        }

        assert(timesStandard.size == timesChunked.size)

        var min = timesStandard.min
        var max = timesStandard.max
        var mean = timesStandard.sum / timesStandard.size

        writer.write("\tStandard min time: " + min + " nanoseconds / " + min / math.pow(10, 6) + " milliseconds / " + min / math.pow(10, 9) + " seconds \n")
        writer.write("\tStandard max time: " + max + " nanoseconds / " + max / math.pow(10, 6) + " milliseconds / " + max / math.pow(10, 9) + " seconds \n")
        writer.write("\tStandard mean time: " + mean + " nanoseconds / " + mean / math.pow(10, 6) + " milliseconds / " + mean / math.pow(10, 9) + " seconds \n\n")

        min = timesChunked.min
        max = timesChunked.max
        mean = timesChunked.sum / timesChunked.size

        writer.write("\tChunked min time: " + min + " nanoseconds / " + min / math.pow(10, 6) + " milliseconds / " + min / math.pow(10, 9) + " seconds \n")
        writer.write("\tChunked max time: " + max + " nanoseconds / " + max / math.pow(10, 6) + " milliseconds / " + max / math.pow(10, 9) + " seconds \n")
        writer.write("\tChunked mean time: " + mean + " nanoseconds / " + mean / math.pow(10, 6) + " milliseconds / " + mean / math.pow(10, 9) + " seconds \n")
        writer.write("\n")
      }
    }
    writer.write("==========================================")
  }

  def chunkedOnePassVsChunked2pass() {
    println("==========================================")
    println("Chunked 1 pass vs Chunked 2 passes")
    writer.write("==========================================\n")
    writer.write("Chunked 1 pass vs Chunked 2 passes\n")

    for (dirName <- dirList) {
      println(dirName)
      writer.write("------------------------------------\n")
      writer.write(dirName + "\n")
      writer.write("------------------------------------\n")
      for (chunkSize <- maxChunkSizes) {
        writer.write("Warmup... Doing " + warmupIterations + " iterations on max chunk size "+chunkSize+" ...\n")
        for (i <- 0 until warmupIterations) {
          val resChunked1pass = JsonParser.root(new BoundaryReader(NumberParser.number, new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))))

          val buf = new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))
          val x = DumpChunkedIntoString.parse(buf)
          val resChunked2pass = JsonParser.root(new CharSequenceReader(x))
          //Not letting JIT kick the results since we don't use them otherwise, and assert a correct result
          if (!resChunked1pass.get.equals(resChunked2pass.get)) {
            println("not equals")
            throw new AssertionError()
          }
        }
        var timesChunked1pass = new ListBuffer[Long]()
        var timesChunked2pass = new ListBuffer[Long]()

        println("Chunked 1 pass vs Chunked 2 passes of max size " + chunkSize)

        writer.write("Testing... Doing " + realIterations+"...\n\n")
        writer.write("Chunked 1 pass vs Chunked 2 passes of max size " + chunkSize + "\n\n")
        for (i <- 0 until realIterations) {
          //Chunked 1 pass
          val rdr = new BoundaryReader(NumberParser.number, new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader())))

          var startTime = System.nanoTime()
          val resChunked1pass = JsonParser.root(rdr)
          val endTimeStandard = System.nanoTime() - startTime
          timesChunked1pass += endTimeStandard

          //Chunked 2 passes
          val buf = new PagedSeqReader(PagedSeq.fromReader(Source.fromFile(new File(dirName + "/randomChunked" + chunkSize + "Json")).bufferedReader()))
          startTime = System.nanoTime()
          val x = DumpChunkedIntoString.parse(buf)
          val resChunked2pass = JsonParser.root(new CharSequenceReader(x))
          val endTimeChunked = System.nanoTime() - startTime
          timesChunked2pass += endTimeChunked

          if (writeIterations) {
            writer.write("Iteration " + (i + 1) + " =>\n")
            writer.write("\tStandard: " + endTimeStandard + " nanoseconds\n")
            writer.write("\tChunked:  " + endTimeChunked + " nanoseconds\n\n")
          }

          //Not letting JIT kick the results since we don't use them otherwise, and assert a correct result
          if (!resChunked1pass.get.equals(resChunked2pass.get)) {
            println("not equals")
            throw new AssertionError()
          }
        }

        assert(timesChunked1pass.size == timesChunked2pass.size)

        var min = timesChunked1pass.min
        var max = timesChunked1pass.max
        var mean = timesChunked1pass.sum / timesChunked1pass.size

        writer.write("\tChunked 1 pass min time: " + min + " nanoseconds / " + min / math.pow(10, 6) + " milliseconds / " + min / math.pow(10, 9) + " seconds \n")
        writer.write("\tChunked 1 pass max time: " + max + " nanoseconds / " + max / math.pow(10, 6) + " milliseconds / " + max / math.pow(10, 9) + " seconds \n")
        writer.write("\tChunked 1 pass mean time: " + mean + " nanoseconds / " + mean / math.pow(10, 6) + " milliseconds / " + mean / math.pow(10, 9) + " seconds \n\n")

        min = timesChunked2pass.min
        max = timesChunked2pass.max
        mean = timesChunked2pass.sum / timesChunked2pass.size

        writer.write("\tChunked 2 passes min time: " + min + " nanoseconds / " + min / math.pow(10, 6) + " milliseconds / " + min / math.pow(10, 9) + " seconds \n")
        writer.write("\tChunked 2 passes max time: " + max + " nanoseconds / " + max / math.pow(10, 6) + " milliseconds / " + max / math.pow(10, 9) + " seconds \n")
        writer.write("\tChunked 2 passes mean time: " + mean + " nanoseconds / " + mean / math.pow(10, 6) + " milliseconds / " + mean / math.pow(10, 9) + " seconds \n")
        writer.write("\n")
      }
    }
    writer.write("==========================================")
  }
}