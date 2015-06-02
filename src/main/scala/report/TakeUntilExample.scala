package report

import mime.CsvParser
import utils.TakeUntilParser

import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

/**
 * Created by alex on 30.05.15.
 */
object TakeUntilExample extends RegexParsers with TakeUntilParser {

  override def skipWhitespace = false

  def CRLF = "\n"

  def main(arr: Array[String]) = {
    val text = "id,first_name,last_name,email,mac_address\n1,Mary,Vasquez,mvasquez0@shareasale.com,45-FA-15-4A-31-76\n2,Albert,Long,along1@cdc.gov,AD-B2-2B-DA-28-76\n3,Kelly,Freeman,kfreeman2@g.co,42-90-A5-01-6B-25\n\n--0000--\n\n4,Robin,Castillo,rcastillo3@npr.org,F8-A7-4E-8E-82-B1\n5,Janet,Shaw,jshaw4@miibeian.gov.cn,00-19-17-BB-18-66"
    println(text)
    val take: Parser[List[List[String]]] = takeUntil("\n--0000--\n", CsvParser.record.asInstanceOf[Parser[List[String]]] <~ CRLF)
    val list = parse(take, new CharSequenceReader(text)).getOrElse(Nil)
    println(list.mkString("\n"))
  }
}