package parsercombinator

import scala.util.parsing.combinator._

object Arith extends JavaTokenParsers {
  
    def parse(input: CharSequence) = parseAll(expr, input) match {
        case Success(res, _) => res
        case e => throw new RuntimeException(e.toString);
    }
    
    
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)    
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}