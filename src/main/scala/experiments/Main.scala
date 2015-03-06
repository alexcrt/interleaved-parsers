package experiments

/**
 * Created by alex on 05.03.15.
 */
object Main {
  def main(args: Array[String]): Unit = {
    println((OXOParser.oxos ~ OXOParser.eoi)("oxo"))
  }
}
