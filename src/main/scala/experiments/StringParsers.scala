package experiments

/**
 * Created by alex on 05.03.15.
 */
trait StringParsers extends SimpleParsers {
  type Input = String
  type Elem = Char
  private val EOI = 0.toChar

  def first(in: Input): Elem = if(in == "") EOI else in(0)
  def rest(in: Input): Input = if(in == "") in else in.substring(1)
  def eoi = accept(EOI) // accept is now defined in SimpleParsers

}
