package cats.parse.extra

import cats.parse.{Parser, Parser0}

/**
 * Typeclass for "has a Parser0"
 *
 * This is primarily provided to help keep track of `Parser0` instances, and as such the omission of
 * `cats.Functor` instances is intentional.
 * @tparam A
 */
trait DefaultParser0[+A] {
  def parser0: Parser0[A]
}
object DefaultParser0 {
  def apply[A](implicit P: DefaultParser0[A]): P.type = P

  def instance[A](p: Parser0[A]): DefaultParser0[A] = new Impl[A](p)

  private final class Impl[+A](override val parser0: Parser0[A])
    extends DefaultParser0[A]
      with Serializable

  object syntax {
    implicit final class DefaultParser0Ops(private val raw: String) extends AnyVal {
      def parse[A: DefaultParser0]: Either[Parser.Error, (String, A)] =
        DefaultParser0[A].parser0.parse(raw)

      def parseAll[A: DefaultParser0]: Either[Parser.Error, A] =
        DefaultParser0[A].parser0.parseAll(raw)
    }
  }

  implicit def defaultParserIsDefaultParser0[A: DefaultParser]: DefaultParser0[A] =
    DefaultParser0.instance(DefaultParser[A].parser)
}