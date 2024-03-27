package cats.parse.extra

import cats.parse.Parser

/**
 * Typeclass for "has a Parser"
 *
 * This is primarily provided to help keep track of `Parser` instances, and as such the omission of
 * `cats.Functor` instances is intentional.
 * @tparam A
 */
trait DefaultParser[+A] {
  def parser: Parser[A]
}
object DefaultParser {
  def apply[A](implicit P: DefaultParser[A]): P.type = P

  def instance[A](p: Parser[A]): DefaultParser[A] = new Impl[A](p)

  private final class Impl[+A](override val parser: Parser[A])
    extends DefaultParser[A]
      with Serializable

  object syntax {
    implicit final class DefaultParserOps(private val raw: String) extends AnyVal {
      def parse[A: DefaultParser]: Either[Parser.Error, (String, A)] =
        DefaultParser[A].parser.parse(raw)

      def parseAll[A: DefaultParser]: Either[Parser.Error, A] =
        DefaultParser[A].parser.parseAll(raw)
    }
  }
}