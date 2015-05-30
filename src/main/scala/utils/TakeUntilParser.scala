package utils

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by alex on 15.04.15.
 */
trait TakeUntilParser extends RegexParsers {

  def CRLF = "\r\n" | "\n"

  override def skipWhitespace = false

  def takeUntil[T](condition: Parser[Any], parser: Parser[T]): Parser[List[T]] = rep(not(condition) ~> parser)

}
