package chunked


import scala.collection.immutable.PagedSeq
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._


/**
 * Created by alex on 25.04.15.
 */
object StringParsers extends StringParsers {
  override def skipWhitespace = false
}
trait StringParsers extends Parsers {

  type Elem = Char

  protected val whiteSpaces = Set('\t', '\r', '\n', '\f', ' ')

  protected def skipWhitespace = whiteSpaces.nonEmpty

  def digit: Parser[Char] = acceptIf(_.isDigit)(e => "Expected digit but was " + e)

  def integerNumber: Parser[String] = opt("-") ~ rep1(digit) map {
    case Some(minus) ~ nb => minus + nb.mkString
    case None ~ nb => nb.mkString
  }

  def decimalNumber: Parser[String] = opt("-") ~ (decimalNumber0 | decimalNumber1) map {
    case Some(minus) ~ nb => minus + nb
    case None ~ nb => nb
  }

  def decimalNumber0: Parser[String] = rep1(digit) ~ opt("." ~ rep(digit)) map {
    case p1 ~ None => p1.mkString
    case p1 ~ Some(t) => p1.mkString + t._1 + t._2.mkString
  }

  def decimalNumber1: Parser[String] = rep(digit) ~ "." ~ rep1(digit) map {
    case p1 ~ "." ~ p2 => p1.mkString + "." + p2.mkString
  }

  def character: Parser[Char] = acceptIf(e => Character.isDefined(e))(e => "Expected character but was " + e)

  def nonQuoted: Parser[Char] = acceptIf(e => e != '"')(e => "Expected character that was not a quote but was " + e)

  def quoted: Parser[String] = '\\' ~ character map { case '\\' ~ c => "\\" + c }

  def stringLiteral: Parser[String] = "\"" ~> rep1(quoted | nonQuoted) <~ "\"" map (l => "\"" + l.mkString + "\"")

  private def handleWhiteSpace(in: Input): Input = {
    if (!skipWhitespace) {
      in
    } else {
      var in0 = in
      while (!in0.atEnd && whiteSpaces.contains(in0.first)) {
        in0 = in0.rest
      }
       in0
    }
  }

  implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      var consumed = 0
      var rdr = in

      if(skipWhitespace) rdr = handleWhiteSpace(rdr)

      while (consumed < s.length && s.charAt(consumed) == rdr.first) {
        rdr = rdr.rest
        consumed += 1
      }

      if (skipWhitespace) rdr = handleWhiteSpace(rdr)

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
