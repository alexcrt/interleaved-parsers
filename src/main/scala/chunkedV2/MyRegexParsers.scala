package chunkedV2

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers

/**
 * Created by alex on 25.04.15.
 */
trait MyRegexParsers extends RegexParsers {

  val whiteSpaces = Set('\t', '\r', '\n', '\f', ' ')

  def w : Parser[String] = new Parser[String] {
    def apply(in: Input) = Success("", handleWhiteSpace(in))
  }

  def handleWhiteSpace(in: Input): Input = {
    if (!skipWhitespace) {
      in
    }
    else {
      var in0 = in
      while (whiteSpaces.contains(in0.first)) {
        in0 = in0.rest
      }
      in0
    }
  }

  override implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      var consumed = 0
      var rdr = in //handleWhiteSpace(in)

      while (consumed < s.length && s.charAt(consumed) == rdr.first) {
        rdr = rdr.rest
        consumed += 1
      }

      if (consumed == s.length) {
        Success(s, rdr)
      } else {
        Failure("`" + s + "' expected but " + s.charAt(consumed) + " found", rdr)
      }
    }
  }

  /** A parser that matches a regex string */
  override implicit def regex(r: Regex): Parser[String] = throw new UnsupportedOperationException("Regex")
}
