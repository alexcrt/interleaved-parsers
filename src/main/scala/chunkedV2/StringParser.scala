package chunkedV2


import scala.collection.immutable.PagedSeq
import scala.util.parsing.input._
import scala.util.parsing.combinator.Parsers


/**
 * Created by alex on 25.04.15.
 */
trait StringParser extends Parsers {

  type Elem = Char

  protected val skipWhitespace = true
  
  protected val whiteSpaces = Set('\t', '\r', '\n', '\f', ' ')

  def w : Parser[String] = new Parser[String] {
    def apply(in: Input) = Success("", handleWhiteSpace(in))
  }

  def digit: Parser[Char] = acceptIf(e => Character.isDigit(e))(e => "Expected digit but was "+e)

  def integerNumber: Parser[String] = rep1(digit) map (l => l.mkString)

  def decimalNumber:Parser[String] = decimalNumber0 | decimalNumber1

  def decimalNumber0: Parser[String] = rep1(digit) ~ opt("." ~ rep(digit)) map {
    case p1 ~ None => p1.mkString
    case p1 ~ Some(t) => p1.mkString + t._1 + t._2.mkString
  }

  def decimalNumber1: Parser[String] = rep(digit) ~ "." ~ rep1(digit) map {
    case p1 ~ "." ~ p2 => p1.mkString + "." + p2.mkString
  }

  def character: Parser[Char] = acceptIf(e => Character.isDefined(e))(e => "Expected character but was "+e)

  def nonQuoted: Parser[Char] = acceptIf(e => e != '"')(e => "Expected character that was not a quote but was "+e)

  def quoted: Parser[Char] = "\\" ~> character

  def stringLiteral: Parser[String] = "\"" ~> rep1(quoted | nonQuoted) <~ "\"" map (l => l.mkString)

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

  implicit def literal(s: String): Parser[String] = new Parser[String] {
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

  def parse[T](p: Parser[T], in: Reader[Char]): ParseResult[T] =
    p(in)

  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    p(new CharSequenceReader(in))

  def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] =
    p(new PagedSeqReader(PagedSeq.fromReader(in)))

  def parseAll[T](p: Parser[T], in: Reader[Char]): ParseResult[T] =
    parse(phrase(p), in)

  def parseAll[T](p: Parser[T], in: java.io.Reader): ParseResult[T] =
    parse(phrase(p), in)

  def parseAll[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    parse(phrase(p), in)
}
