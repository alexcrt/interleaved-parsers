package experiments

/**
 * Created by alex on 05.03.15.
 */
trait SimpleResults {
    type Input

    trait Result[+T] {
        def next: Input
        def map[U](f: T => U): Result[U]
        def flatMapWithNext[U](f: T => Input => Result[U]): Result[U]
        def append[U >: T](alt: => Result[U]): Result[U]
    }

    case class Success[+T](result: T, next:Input) extends Result[T] {
        override def map[U](f: T => U) = Success(f(result), next)
        override def flatMapWithNext[U](f: T => Input => Result[U]) = f(result)(next)
        override def append[U >: T](alt: => Result[U]): Result[U] = this
    }
    case class Failure(msg: String, next: Input) extends Result[Nothing] {
      def map[U](f: Nothing => U) = this
      def flatMapWithNext[U](f: Nothing => Input => Result[U]) = this
      def append[U](alt: => Result[U]) = alt
    }
}
