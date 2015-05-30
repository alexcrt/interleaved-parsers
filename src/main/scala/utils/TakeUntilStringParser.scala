package utils

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by alex on 28.05.15.
 */
trait TakeUntilStringParser extends RegexParsers {


  def CRLF = "\r\n" | "\n"

  override def skipWhitespace = false

  def takeUntilIntoString(condition: Parser[Any], parser: Parser[String]): Parser[String] = repIntoString(not(condition) ~> parser)

  def repIntoString(p: => Parser[String]): Parser[String] = repIntoStringBuilder(p) | success("")

  def repNIntoString(num: Int, p: => Parser[String]): Parser[String] =
    if (num == 0) success("") else Parser { in =>
      val elems = new StringBuilder()
      val p0 = p    // avoid repeatedly re-evaluating by-name parser

      @tailrec def applyp(in0: Input): ParseResult[String] =
        if (elems.length == num) Success(elems.toString(), in0)
        else p0(in0) match {
          case Success(x, rest) => elems.append(x); applyp(rest)
          case ns: NoSuccess    => ns
        }

      applyp(in)
    }

  private def repIntoStringBuilder(p: => Parser[String]): Parser[String] = rep0IntoStringBuider(p, p)

  private def rep0IntoStringBuider(first: => Parser[String], p0: => Parser[String]): Parser[String] = Parser { in =>
    lazy val p = p0 // lazy argument
    val elems = new StringBuilder()

    def continue(in: Input): ParseResult[String] = {
      val p0 = p // avoid repeatedly re-evaluating by-name parser
      @tailrec def applyp(in0: Input): ParseResult[String] = p0(in0) match {
          case Success(x, rest) => elems.append(x); applyp(rest)
          case e@Error(_, _) => e // still have to propagate error
          case _ => Success(elems.toString(), in0)
        }

      applyp(in)
    }

    first(in) match {
      case Success(x, rest) => elems.append(x); continue(rest)
      case ns: NoSuccess => ns
    }
  }

}
