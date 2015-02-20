package parsercombinator

import scala.util.parsing.combinator._

class Arith(input: CharSequence) extends JavaTokenParsers {
  
    def parse(): ParseResult[Any] = {
        parseAll(expr, input)
    }
    
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)    
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}