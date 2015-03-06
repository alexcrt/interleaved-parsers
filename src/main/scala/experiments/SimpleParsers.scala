package experiments

/**
 * Created by alex on 05.03.15.
 */
trait SimpleParsers extends SimpleResults {
  type Elem

  implicit def accept(expected: Elem)

  abstract class Parser[+T] extends (Input => Result[T]) {
    def apply(in: Input): Result[T]

    def flatMap[U](f: T => Parser[U]): Parser[U] = new Parser[U] {
      def apply(in: Input) = Parser.this(in) flatMapWithNext (f)
    }

    def map[U](f: T => U): Parser[U] = new Parser[U] {
      def apply(in: Input) = Parser.this(in) map (f)
    }

    def |[U >: T](p: => Parser[U]): Parser[U] = new Parser[U] {
      def apply(in: Input) = Parser.this(in) append p(in)
    }

    def ~[U](p: => Parser[U]): Parser[(T, U)] = for (a <- this; b <- p) yield (a, b)
  }

}

