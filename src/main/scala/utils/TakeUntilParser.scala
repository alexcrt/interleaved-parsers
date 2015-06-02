package utils

import scala.annotation.tailrec
import scala.util.parsing.combinator.{Parsers, RegexParsers}

/**
 * Created by alex on 15.04.15.
 */
trait TakeUntilParser extends Parsers {
  def takeUntil[T](condition: Parser[Any], parser: Parser[T]): Parser[List[T]] = rep(not(condition) ~> parser)
}
