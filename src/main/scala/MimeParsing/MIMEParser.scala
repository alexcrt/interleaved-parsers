package MimeParsing

import java.io.Reader

import scala.util.parsing.combinator.RegexParsers

object MIMEParser extends RegexParsers {

  override def skipWhitespace = false

  val MimeVersion = "MIME-Version:"
  val ContentType = "Content-Type:"

  def CRLF = "\r\n" | "\n"

  def versionNumber = """\d+.\d+""".r

  def parse(input: Reader) = parseAll(all, input) match {
    case Success(res, _) => res
    case e => throw new RuntimeException(e.toString);
  }

  def all: Parser[Mime] = header ~ content() map (t => new Mime(t._1, t._2))

  def header: Parser[MimeHeader] = MimeVersion ~> versionNumber <~ CRLF map (v => new MimeHeader(v))

  def content(multipart: Boolean = false, boundary: Option[String] = None): Parser[Any] = ContentType ~> ((contentType <~ "/") ~ subType) <~";" flatMap (t => getParser((t._1, t._2), multipart, boundary))

  def contentType = "application" | "audio" | "image" | "message" | "multipart" | "text" | "video"
  def subType = "json" | "plain" | "mixed"

  def getParser(t: (String, String), multipart: Boolean, boundary: Option[String] = None): Parser[Any] = t match {
    case ("application", subtype) => throw new Exception ("Not implemented yet")

    case ("text", subtype) => CRLF ~> getTextParser(subtype, boundary) <~ opt(CRLF)

    case ("audio", _) => throw new Exception ("Not implemented yet")

    case ("image", _) => throw new Exception ("Not implemented yet")

    case ("message", _) => throw new Exception ("Not implemented yet")

    case ("multipart", subtype) =>
      if(multipart)
        throw new Exception ("Multipart already parsed")
      else
        MultipartOptions.valueOf(subtype) match {
          case Some(option) => getMultipartParser(option)
          case None => throw new Exception ("Only mixed and digest are supported for multipart")
      }

    case ("video", _) => throw new Exception ("Not implemented yet")

    case _ => throw new Exception("Cannot parse such type: " + t)
  }

  def getTextParser(subtype: String, boundary: Option[String] = None): Parser[Any] = subtype match {
    case("json") => JsonParser.root.asInstanceOf[Parser[Any]]
    case ("plain") => TextParser.root(boundary).asInstanceOf[Parser[Any]]
    case _ => throw new Exception("Cannot parse such text subtype: " + subtype)
  }

  def getMultipartParser(value: MultipartOptions.Value): Parser[Any] =  value match {
    case MultipartOptions.Mixed => "boundary=" ~> '"' ~> """\w+""".r <~ '"' <~ CRLF flatMap (x => "--" ~> x ~> CRLF ~> repsep(content(true, Some("--"+x)), "--" ~> x <~ CRLF) <~ "--" <~ x <~ opt(CRLF))
    case MultipartOptions.Digest => throw new Exception("Digest not implemented yet")
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
    override def toString = List(header.toString, content).mkString("\n")
  }

  final class MimeHeader(versionNumber : String) {
    override def toString = MimeVersion + versionNumber
  }
}