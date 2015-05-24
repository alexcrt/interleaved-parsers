package chunked

import utils.TakeUntilParser

import scala.collection.immutable.PagedSeq
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._


/**
 * Created by alex on 25.04.15.
 */
object DumpChunkedIntoString extends TakeUntilParser {

    def parse(input: Reader[Char]) = parseAll(dump, input) match {
      case Success(e, rdr) => e
      case err => throw new RuntimeException(err.toString)
    }

    def cond: Parser[String] = "\n0\n"

    def p: Parser[String] = NumberParser.number.asInstanceOf[Parser[Int]] flatMap (n => repN(n, ".".r) map (_.mkString))

    def dump: Parser[String] = takeUntil(cond, p) map (l => l.mkString)
}