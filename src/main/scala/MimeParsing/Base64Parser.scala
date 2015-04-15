package MimeParsing

import java.io.Reader

import scala.util.parsing.combinator._

object Base64Parser extends TakeUntilParser {

    val decoder = java.util.Base64.getMimeDecoder

    def DATA = """(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?""".r

    def root(boundary: Option[String] = None): Parser[List[String]] = boundary match {
        case Some(bound) => takeUntil(bound, DATA <~ CRLF) map (list => list.map(decoder.decode).map(new String(_)).mkString.split("\n").toList)
        case None => repsep(DATA, CRLF) map (list => list.map(decoder.decode).map(new String(_)).mkString.split("\n").toList)
    }
}