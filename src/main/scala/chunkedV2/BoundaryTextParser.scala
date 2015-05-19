package chunkedV2

import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Reader, CharSequenceReader}

/**
 * Created by alex on 24.04.15.
 */
object BoundaryTextParser extends StringParser {

  def parseBonjour: Parser[String] = "bonjour"

}
