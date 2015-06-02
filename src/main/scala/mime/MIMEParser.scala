package mime

import java.io.Reader
import utils.TakeUntilParser

import scala.util.parsing.combinator.RegexParsers

object MIMEParser extends RegexParsers with TakeUntilParser {

  override protected val whiteSpace = """[ ]+""".r

  def mimeVersionValue = "MIME-Version" ~> ":"
  def contentTypeValue = "Content-Type" ~> ":"
  def contentTransferEncodingValue = "Content-transfer-encoding" ~> ":"

  def CRLF = "\r\n" | "\n"
  def CRLFMore = rep1(CRLF)

  def versionNumber = """\d+.\d+""".r

  def parse(input: Reader) = parseAll(all, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString);
  }

  def all: Parser[Mime] = header ~ content() map (t => new Mime(t._1, t._2))

  def header: Parser[MimeHeader] = mimeVersionValue ~> versionNumber <~ CRLFMore map (v => new MimeHeader(v))

  def content(multipart: Boolean = false, boundary: Option[String] = None): Parser[Any] =
    contentTypeValue ~> ((contentType <~ "/") ~ subType) <~ ";" flatMap (t => getParser((t._1, t._2), multipart, boundary))

  def contentType = "application" | "audio" | "image" | "message" | "multipart" | "text" | "video"

  def subType = "json" | "plain" | "csv" | "mixed" | "octet-stream"

  def contentTransferEncoding = "base64"

  def getParser(t: (String, String), multipart: Boolean, boundary: Option[String] = None): Parser[Any] = t match {

    case ("text", subtype) => CRLFMore ~> getTextParser(subtype, boundary) <~ opt(CRLFMore)

    case ("multipart", subtype) =>
      if(multipart)
        throw new Exception ("Multipart already parsed")
      else
        MultipartOptions.valueOf(subtype) match {
          case Some(option) => getMultipartParser(option)
          case None => throw new Exception ("Only mixed and digest are supported for multipart")
      }

    case _ => throw new Exception("Cannot parse such type: " + t)
  }

  def getTextParser(subtype: String, boundary: Option[String] = None): Parser[Any] = subtype match {
    case ("json") => JsonParser.root.asInstanceOf[Parser[Any]]
    case ("plain") => boundary match {
      case Some(bound) => takeUntil(rep(CRLF) ~> bound, ".*".r <~ CRLF) map (s => s.mkString("\n"))
      case None => regex("(?s).*".r)
    }
    case ("csv") => boundary match {
      case Some(bound) => takeUntil(rep(CRLF) ~> bound, CsvParser.record.asInstanceOf[Parser[Any]]  <~ CRLF)
      case None => CsvParser.file.asInstanceOf[Parser[Any]]
    }
    case _ => throw new Exception("Cannot parse such text subtype: " + subtype)
  }

  def getMultipartParser(value: MultipartOptions.Value): Parser[Any] =  value match {
    case MultipartOptions.Mixed =>
      "boundary=" ~> '"' ~> """\w+""".r <~ '"' <~ CRLFMore flatMap (x => "--" ~> x ~> CRLFMore ~> repsep(content(true, Some("--"+x)), "--" ~> x <~ CRLFMore) <~ "--" <~ x <~ opt(CRLFMore))
    case MultipartOptions.Digest =>
      throw new Exception("Digest not implemented yet")
  }

  /**
   * Classes to have a typed parser
   */
  object MultipartOptions extends Enumeration {
    type MultipartOptions = Value
    val Mixed = Value("mixed")
    val Digest = Value("digest")

    def valueOf(name: String) = values.find(_.toString == name) 
  }

  final class Mime(header : MimeHeader, content: Any) {
    override def toString = List(header, content).mkString("\n")
  }

  final class MimeHeader(versionNumber : String) {
    override def toString = "Mime-Version:" + versionNumber
  }
}