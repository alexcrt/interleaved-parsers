package interleavedV3

import scala.reflect.internal.util.Statistics.SubQuantity
import scala.util.control.Breaks._
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

/**
 * Created by alex on 25.04.15.
 */
trait MyRegexParsers extends RegexParsers {

  override implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      var consumed = 0
      var rdr = in

      breakable {
        while (consumed < s.length) {
          val first = rdr.first
          if (s.charAt(consumed) != first) {
            break
          }
          consumed += 1
          rdr = rdr.rest
        }
      }
      if(consumed == s.length) {
        Success(s, rdr)
      } else {
        Failure("`"+s+"' expected but "+s.charAt(consumed)+" found", rdr)
      }
    }
  }

  /** A parser that matches a regex string */
  override implicit def regex(r: Regex): Parser[String] = throw new UnsupportedOperationException("Regex")
}
