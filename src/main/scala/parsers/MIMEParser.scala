package parsers

import java.io.Reader

import scala.util.parsing.combinator.RegexParsers


object MIMEParser extends RegexParsers {

  val MimeVersion = "MIME-Version:"
  def versionNumber = """\d+.\d+""".r

  def parse(input: Reader) = parseAll(header, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString);
  }

  def header: Parser[String] = MimeVersion ~> versionNumber
}
