package parsercombinator

import scala.util.parsing.combinator._

object Arith extends JavaTokenParsers {
  
    def parse(input: CharSequence): ParseResult[Any] = {
        parseAll(expr, input)
    }
    
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)    
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}