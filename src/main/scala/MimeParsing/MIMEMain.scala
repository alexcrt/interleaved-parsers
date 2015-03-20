package MimeParsing

import java.io.{FileReader, File, StringReader}

import scala.io.Source

/**
 * Created by alex on 19.03.15.
 */
object MIMEMain {
  def main(args: Array[String]): Unit = {
    val reader = Source.fromFile("testing_files/mime_messages/text").bufferedReader()
    val res = MIMEParser.parse(reader)
    println(res)

  }
}
