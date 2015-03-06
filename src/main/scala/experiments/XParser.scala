package experiments

/**
 * Created by alex on 05.03.15.
 */
object XParser extends SimpleResults {
  type Input = String
  val accept: (Input, Char) => Result[Char] = {
    (in: String, char: Char) =>
    if(in.isEmpty)
      Failure("empty input", in)
    else if (in.charAt(0) == char)
      Success(char, in.substring(1))
    else
      Failure("expected an "+char, in)
  }
}
