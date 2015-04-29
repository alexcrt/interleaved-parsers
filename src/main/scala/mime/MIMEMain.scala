package mime

import java.io.{FileReader, File, StringReader}

import scala.io.Source

/**
 * Created by alex on 19.03.15.
 */
object MIMEMain {
  def main(args: Array[String]): Unit = {
    List("testing_files/mime_messages/text", "testing_files/mime_messages/json", "testing_files/mime_messages/boundary")
      .drop(2)
      .map(Source.fromFile(_).bufferedReader)
      .map(MIMEParser.parse)
      .foreach(res => println(res+"\n=================="))

  }
}
