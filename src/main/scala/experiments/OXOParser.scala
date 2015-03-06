package experiments

/**
 * Created by alex on 05.03.15.
 */
object OXOParser extends StringParsers {

  def oxo = 'o' ~ 'x' ~ 'o'
  def oxos:Parser[Any] = oxo ~ ' ' ~ oxos | oxo
}
