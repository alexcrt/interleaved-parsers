package mime

import java.io.Reader

import mime.MIMEParser._

import scala.util.parsing.combinator.RegexParsers

object ApplicationParser extends RegexParsers {
    def apply(input: Reader, application: String) = {
      println("HERE")
      println(application)
    }

   def parse(input: Reader) = parseAll(content, input) match {
     case Success(res, _) => res
     case e => throw new RuntimeException(e.toString);
   }

   def content:Parser[String] = "JSON"
}
