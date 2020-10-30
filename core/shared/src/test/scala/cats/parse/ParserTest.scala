package cats.parse

import cats.{Eq, Id, FlatMap, Functor, Defer, MonoidK, Monad, Eval}
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Cogen}

import cats.implicits._

sealed abstract class GenT[F[_]] { self =>
  type A
  val cogen: Cogen[A]
  val fa: F[A]

  def transform[G[_]](fk: FunctionK[F, G]): GenT[G] =
    new GenT[G] {
      type A = self.A
      val cogen = self.cogen
      val fa: G[A] = fk(self.fa)
    }

  def toId(implicit F: Functor[F]): F[GenT[Id]] =
    F.map(fa) { a =>
      new GenT[Id] {
        type A = self.A
        val cogen = self.cogen
        val fa = a
      }
    }

  override def toString: String = s"GenT($fa)"
}

object GenT {
  def apply[F[_], A0: Cogen](pa: F[A0]): GenT[F] =
    new GenT[F] {
      type A = A0
      val cogen = implicitly[Cogen[A0]]
      val fa = pa
    }
}

object ParserGen {
  implicit val functorGen: Functor[Gen] =
    new Functor[Gen] {
      def map[A, B](ga: Gen[A])(fn: A => B) = ga.map(fn)
    }

  def arbGen[A: Arbitrary: Cogen]: GenT[Gen] =
    GenT(Arbitrary.arbitrary[A])

  val pures: Gen[GenT[Gen]] =
    Gen.oneOf(arbGen[Int], arbGen[Boolean], arbGen[String], arbGen[(Int, Int)])

  val expect0: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[String].map { str =>
      GenT(Parser.string(str))
    }

  val charIn: Gen[GenT[Parser]] =
    Gen.oneOf(
      Arbitrary.arbitrary[List[Char]].map { cs =>
        GenT(Parser.charIn(cs): Parser[Char])
      },
      Gen.const(GenT(Parser.anyChar: Parser[Char])))

  val charIn1: Gen[GenT[Parser1]] =
    Gen.oneOf(
      Arbitrary.arbitrary[List[Char]].map { cs =>
        GenT(Parser.charIn(cs))
      },
      Gen.const(GenT(Parser.anyChar)))

  val expect1: Gen[GenT[Parser1]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser.fail: Parser1[Unit])
      else GenT(Parser.string1(str))
    }

  val fail: Gen[GenT[Parser]] =
    Gen.const(GenT(Parser.fail: Parser[Unit]))

  def void(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.void(g.fa))

  def void1(g: GenT[Parser1]): GenT[Parser1] =
    GenT(Parser.void1(g.fa))

  def string(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.string(g.fa))

  def string1(g: GenT[Parser1]): GenT[Parser1] =
    GenT(Parser.string1(g.fa))

  def backtrack(g: GenT[Parser]): GenT[Parser] =
    GenT(g.fa.backtrack)(g.cogen)

  def backtrack1(g: GenT[Parser1]): GenT[Parser1] =
    GenT(g.fa.backtrack)(g.cogen)

  def defer(g: GenT[Parser]): GenT[Parser] =
    GenT(Defer[Parser].defer(g.fa))(g.cogen)

  def defer1(g: GenT[Parser1]): GenT[Parser1] =
    GenT(Defer[Parser1].defer(g.fa))(g.cogen)

  def rep(g: GenT[Parser1]): GenT[Parser] = {
    implicit val cg = g.cogen
    GenT[Parser, List[g.A]](g.fa.rep)
  }

  def rep1(g: GenT[Parser1]): GenT[Parser1] = {
    implicit val cg = g.cogen
    GenT[Parser1, List[g.A]](g.fa.rep1.map(_.toList))
  }

  def product(ga: GenT[Parser], gb: GenT[Parser]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].product(ga.fa, gb.fa)),
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].map2(ga.fa, gb.fa)((_, _))),
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].map2Eval(ga.fa, Eval.later(gb.fa))((_, _)).value),
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].map2Eval(ga.fa, Eval.now(gb.fa))((_, _)).value)
    )
  }

  def softProduct(ga: GenT[Parser], gb: GenT[Parser]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.const(
      GenT[Parser, (ga.A, gb.A)](ga.fa.soft ~ gb.fa),
    )
  }

  def product1(ga: GenT[Parser1], gb: GenT[Parser1]): Gen[GenT[Parser1]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser1, (ga.A, gb.A)](FlatMap[Parser1].product(ga.fa, gb.fa)),
      GenT[Parser1, (ga.A, gb.A)](FlatMap[Parser1].map2(ga.fa, gb.fa)((_, _))),
      GenT[Parser1, (ga.A, gb.A)](FlatMap[Parser1].map2Eval(ga.fa, Eval.later(gb.fa))((_, _)).value),
      GenT[Parser1, (ga.A, gb.A)](FlatMap[Parser1].map2Eval(ga.fa, Eval.now(gb.fa))((_, _)).value),
    )
  }

  def product10(ga: GenT[Parser1], gb: GenT[Parser]): Gen[GenT[Parser1]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser1, (ga.A, gb.A)](Parser.product10(ga.fa, gb.fa)),
      GenT[Parser1, ga.A](ga.fa <* gb.fa),
      GenT[Parser1, gb.A](ga.fa *> gb.fa)
    )
  }

  def softProduct1(ga: GenT[Parser1], gb: GenT[Parser]): Gen[GenT[Parser1]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser1, (ga.A, gb.A)](ga.fa.soft ~ gb.fa),
      GenT[Parser1, (gb.A, ga.A)](gb.fa.with1.soft ~ ga.fa),
    )
  }

  def mapped(ga: GenT[Parser]): Gen[GenT[Parser]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.flatMap { fn =>
        Gen.oneOf(
          GenT(ga.fa.map(fn)),
          GenT(FlatMap[Parser].map(ga.fa)(fn))
        )
      }
    }
  }

  def mapped1(ga: GenT[Parser1]): Gen[GenT[Parser1]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.flatMap { fn =>
        Gen.oneOf(
          GenT(ga.fa.map(fn)),
          GenT(FlatMap[Parser1].map(ga.fa)(fn))
        )
      }
    }
  }

  abstract class FlatMap[F[_], B] {
    type A
    val init: F[A]
    val fn: A => F[B]
  }

  def flatMapped(ga: Gen[GenT[Parser]]): Gen[GenT[Parser]] =
    Gen.zip(ga, pures).flatMap { case (parser, genRes) =>
      val genPR: Gen[Parser[genRes.A]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => genRes.A] =
            Gen.function1(genRes.fa)(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[parser.A => Parser[genRes.A]] =
        Gen.function1(genPR)(parser.cogen)

      gfn.flatMap { fn =>
        Gen.oneOf(
          GenT(parser.fa.flatMap(fn))(genRes.cogen),
          GenT(FlatMap[Parser].flatMap(parser.fa)(fn))(genRes.cogen)
        )
      }
    }

  // if we use a Parser here, we could loop forever parsing nothing
  def tailRecM(ga: Gen[GenT[Parser1]]): Gen[GenT[Parser]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn = Gen.function1(genPR)(genRes1.cogen)

      Gen.zip(genRes1.fa, gfn)
        .map { case (init, fn) =>
          GenT(Monad[Parser].tailRecM(init)(fn))(genRes2.cogen),
        }
    }

  def tailRecM1(ga: Gen[GenT[Parser1]]): Gen[GenT[Parser1]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser1[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn = Gen.function1(genPR)(genRes1.cogen)

      Gen.zip(genRes1.fa, gfn)
        .map { case (init, fn) =>
          GenT(FlatMap[Parser1].tailRecM(init)(fn))(genRes2.cogen),
        }
    }


  def flatMapped1(ga: Gen[GenT[Parser]], ga1: Gen[GenT[Parser1]]): Gen[GenT[Parser1]] =
    Gen.zip(ga, ga1, pures).flatMap { case (parser, parser1, genRes) =>
      val genPR: Gen[Parser1[genRes.A]] = {
        ga1.flatMap { init =>
          val mapFn: Gen[init.A => genRes.A] =
            Gen.function1(genRes.fa)(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[parser.A => Parser1[genRes.A]] =
        Gen.function1(genPR)(parser.cogen)

      val gfn1: Gen[parser1.A => Parser1[genRes.A]] =
        Gen.function1(genPR)(parser1.cogen)


      Gen.frequency(
        (2, gfn1.flatMap { fn =>
          Gen.oneOf(
            GenT(parser1.fa.flatMap(fn))(genRes.cogen), // 1 -> 0
            GenT(FlatMap[Parser1].flatMap(parser1.fa)(fn))(genRes.cogen) // 1 -> 1
          )
        }),
        (1, gfn.map { fn =>
            GenT(parser.fa.with1.flatMap(fn))(genRes.cogen) // 0 -> 1
          })
      )
    }

  def orElse(ga: GenT[Parser], gb: GenT[Parser], res: GenT[Gen]): Gen[GenT[Parser]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).flatMap { case (f1, f2) =>
      Gen.oneOf(
        GenT(ga.fa.map(f1).orElse(gb.fa.map(f2))),
        GenT(MonoidK[Parser].combineK(ga.fa.map(f1), gb.fa.map(f2)))
      )
    }
  }

  def orElse1(ga: GenT[Parser1], gb: GenT[Parser1], res: GenT[Gen]): Gen[GenT[Parser1]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).flatMap { case (f1, f2) =>
      Gen.oneOf(
        GenT(ga.fa.map(f1).orElse1(gb.fa.map(f2))),
        GenT(MonoidK[Parser1].combineK(ga.fa.map(f1), gb.fa.map(f2)))
      )
    }
  }

  // Generate a random parser
  lazy val gen: Gen[GenT[Parser]] = {
    val rec = Gen.lzy(gen)

    Gen.frequency(
      (3, pures.flatMap(_.toId).map(_.transform(new FunctionK[Id, Parser] {
        def apply[A](g: Id[A]): Parser[A] = Parser.pure(g)
      }))),
     (5, expect0),
     (5, charIn),
     (1, Gen.oneOf(GenT(Parser.start), GenT(Parser.end), GenT(Parser.index))),
     (1, rec.map(void(_))),
     (1, rec.map(string(_))),
     (1, rec.map(backtrack(_))),
     (1, rec.map(defer(_))),
     (1, rec.map { gen => GenT(!gen.fa) }),
     (1, Gen.lzy(gen1.map(rep(_)))),
     (1, rec.flatMap(mapped(_))),
     (1, tailRecM(Gen.lzy(gen1))),
     (1, Gen.choose(0, 10).map { l => GenT(Parser.length(l)) }),
     (1, flatMapped(rec)),
     (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => product(g1, g2) }),
     (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => softProduct(g1, g2) }),
     (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse(g1, g2, p) })
    )
  }

  // Generate a random parser
  lazy val gen1: Gen[GenT[Parser1]] = {
    val rec = Gen.lzy(gen1)

    Gen.frequency(
     (8, expect1),
     (8, charIn1),
     (1, Gen.choose(Char.MinValue, Char.MaxValue).map { c => GenT(Parser.char(c)) }),
     (2, rec.map(void1(_))),
     (2, rec.map(string1(_))),
     (2, rec.map(backtrack1(_))),
     (1, rec.map(defer1(_))),
     (1, rec.map(rep1(_))),
     (1, rec.flatMap(mapped1(_))),
     (1, flatMapped1(gen, rec)),
     (1, tailRecM1(rec)),
     (1, Gen.choose(1, 10).map { l => GenT(Parser.length1(l)) }),
     (2, Gen.frequency(
       (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => product1(g1, g2) }),
       (1, Gen.zip(rec, gen).flatMap { case (g1, g2) => product10(g1, g2) }),
       (1, Gen.zip(rec, gen).flatMap { case (g1, g2) => softProduct1(g1, g2) }),
       (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse1(g1, g2, p) })
     ))
    )
  }

}

class ParserTest extends munit.ScalaCheckSuite {

  val tests: Int = if (BitSetUtil.isScalaJs) 50 else 1000

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(tests)
      .withMaxDiscardRatio(10)

  def parseTest[A: Eq](p: Parser[A], str: String, a: A) =
    p.parse(str) match {
      case Right((_, res)) =>
        assert(Eq[A].eqv(a, res), s"expected: $a got $res")
      case Left(errs) =>
        assert(false, errs.toString)
    }

  def parseFail[A: Eq](p: Parser[A], str: String) =
    p.parse(str) match {
      case Right(res) =>
        assert(false, s"expected to not parse, but found: $res")
      case Left(errs) =>
        assert(true)
    }

  test("pure works") {
    parseTest(Parser.pure(42), "anything", 42)
  }

  val fooP = Parser.string1("foo")
  val barP = Parser.string1("bar")

  test("string tests") {
    parseTest(fooP, "foobar", ())
    parseFail(fooP, "bar")

    parseTest(Parser.oneOf1(fooP :: barP :: Nil), "bar", ())
    parseTest(Parser.oneOf1(fooP :: barP :: Nil), "foo", ())
  }

  test("product tests") {
    parseTest(Parser.product01(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product10(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product(fooP, barP), "foobar", ((), ()))
  }

  // we can use String => Iterable[Char]
  val digit = Parser.charIn("0123456789")
  // or use a range which reads a bit nicer:
  val digit1 = Parser.charIn('1' to '9')
  def maybeNeg[A](p1: Parser1[A]): Parser1[String] =
     (Parser.char('-').?.with1 ~ p1).string

  val bigIntP =
    maybeNeg(
      ((digit1 ~ Parser.rep(digit)).void)
        .orElse1(Parser.char('0'))
    )
    .map(BigInt(_))

  property("test an example with BigInt") {
    forAll { bi: BigInt =>
      parseTest(bigIntP, bi.toString, bi)
    }
  }

  property("Parser.start and end work") {
    forAll { (s: String) =>
      if (s.isEmpty) {
        intercept[IllegalArgumentException] {
          Parser.string1(s)
        }
      }
      else {
        val pa = Parser.string1(s)
        assertEquals((Parser.start ~ pa ~ Parser.end).void.parse(s), Right(("", ())))
        assert((pa ~ Parser.start).parse(s).isLeft)
        assert((Parser.end ~ pa).parse(s).isLeft)
        assertEquals((Parser.index ~ pa ~ Parser.index).map { case ((s, _), e) => e - s }.parse(s), Right(("", s.length)))
      }

      true
    }
  }

  property("Parser.length succeeds when the string is long enough") {
    forAll { (s: String, len: Int) =>
      if (len < 1) {
        intercept[IllegalArgumentException] {
          Parser.length1(len)
        }
        assertEquals(Parser.length(len).parse(s), Right((s, "")))
      }
      else {
        val pa = Parser.length(len)
        val pa1 = Parser.length1(len)

        val res = pa.parse(s)
        val res1 = pa1.parse(s)

        assertEquals(res, res1)

        res match {
          case Right((rest, first)) =>
            if (s.length >= len) {
              assertEquals(s.take(len), first)
              assertEquals(s.drop(len), rest)
            }
            else fail(s"expected to not parse: $rest, $first")
          case Left(Parser.Error(0, NonEmptyList(Parser.Expectation.Length(off, l, a), Nil))) =>
            assertEquals(off, 0)
            assertEquals(l, len)
            assertEquals(a, s.length)
          case Left(other) =>
            fail(s"unexpected error: $other")
        }
      }

      true
    }
  }

  property("voided only changes the result") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.parse(str)
      val r2 = genP.fa.void.parse(str)
      val r3 = FlatMap[Parser].void(genP.fa).parse(str)
      val r4 = genP.fa.as(()).parse(str)

      assertEquals(r2, r1.map { case (off, _) => (off, ()) })
      assertEquals(r2, r3)
      assertEquals(r2, r4)
    }
  }

  property("voided only changes the result Parser1") {
    forAll(ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.parse(str)
      val r2 = genP.fa.void.parse(str)
      val r3 = FlatMap[Parser1].void(genP.fa).parse(str)
      val r4 = genP.fa.as(()).parse(str)
      val r5 = ((genP.fa.void: Parser[Unit]) <* Monad[Parser].unit).parse(str)

      assertEquals(r2, r1.map { case (off, _) => (off, ()) })
      assertEquals(r2, r3)
      assertEquals(r2, r4)
      assertEquals(r2, r5)
    }
  }

  property("expected in errors gives valid offsets") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      genP.fa.parse(str) match {
        case Left(err) =>
          err.offsets.forall { off =>
            (0 <= off) && (off <= str.length)
          }
        case Right(_) => true
      }

    }
  }

  property("oneOf nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen), Gen.listOf(ParserGen.gen), Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      val oneOf1 = Parser.oneOf((genP1 ::: genP2).map(_.fa))
      val oneOf2 = Parser.oneOf(genP1.map(_.fa)).orElse(
        Parser.oneOf(genP2.map(_.fa)))

      assertEquals(oneOf1.parse(str), oneOf2.parse(str))
    }
  }

  property("oneOf1 nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen1), Gen.listOf(ParserGen.gen1), Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      val oneOf1 = Parser.oneOf1((genP1 ::: genP2).map(_.fa))
      val oneOf2 = Parser.oneOf1(genP1.map(_.fa)).orElse1(
        Parser.oneOf1(genP2.map(_.fa))
      )

      assertEquals(oneOf1.parse(str), oneOf2.parse(str))
    }
  }

  def orElse[A](p1: Parser[A], p2: Parser[A], str: String): Either[Parser.Error, (String, A)] = {
    if (p1 == Parser.Fail) p2.parse(str)
    else if (p2 == Parser.Fail) p1.parse(str)
    else p1.parse(str) match {
      case left@Left(err) =>
        if (err.failedAtOffset == 0) {
          p2.parse(str)
            .leftMap { err1 =>
              if (err1.failedAtOffset == 0) {
                val errs = err.expected ::: err1.expected
                Parser.Error(err1.failedAtOffset, Parser.Expectation.unify(errs))
              }
              else err1
            }
        }
        else left
      case right => right
    }
  }

  property("oneOf composes as expected") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      assertEquals(genP1.fa.orElse(genP2.fa).parse(str), orElse(genP1.fa, genP2.fa, str))
    }
  }

  property("oneOf1 composes as expected") {
    forAll(ParserGen.gen1, ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      assertEquals(genP1.fa.orElse1(genP2.fa).parse(str), orElse(genP1.fa, genP2.fa, str))
    }
  }

  property("oneOf same as foldLeft(fail)(_.orElse(_))") {
    forAll(Gen.listOf(ParserGen.gen), Arbitrary.arbitrary[String]) { (genP1, str) =>
      val oneOfImpl = genP1.foldLeft(Parser.fail: Parser[Any]) { (leftp, p) => leftp.orElse(p.fa) }

      assertEquals(oneOfImpl.parse(str), Parser.oneOf(genP1.map(_.fa)).parse(str))
    }
  }

  property("oneOf1 same as foldLeft(fail)(_.orElse1(_))") {
    forAll(Gen.listOf(ParserGen.gen1), Arbitrary.arbitrary[String]) { (genP1, str) =>
      val oneOfImpl = genP1.foldLeft(Parser.fail[Any]) { (leftp, p) => leftp.orElse1(p.fa) }

      assertEquals(oneOfImpl.parse(str), Parser.oneOf1(genP1.map(_.fa)).parse(str))
    }
  }

  property("string can be recovered with index") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.string.parse(str)
      val r2 = (genP.fa ~ Parser.index).map { case (_, end) => str.substring(0, end) }.parse(str)

      assertEquals(r1, r2)
    }
  }

  property("backtrack orElse pure always succeeds") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.orElse(Parser.pure(())): Parser[Any]

      assert(p1.parse(str).isRight)
    }
  }

  property("backtrack.? pure always succeeds") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.?

      assert(p1.parse(str).isRight)
    }
  }

  property("a.backtrack either succeeds or fails at 0") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      a.fa.backtrack.parse(str) match {
        case Right(_) => ()
        case Left(err) => assertEquals(err.failedAtOffset, 0)
      }
    }
  }

  property("a ~ b composes as expected") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa ~ p2.fa
      val cres = composed.parse(str)

      val composed1 = Monad[Parser].product(p1.fa, p2.fa)
      val cres1 = composed1.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length(off) ~ p2.fa).map(_._2)
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a ~ b composes as expected parser1") {
    forAll(ParserGen.gen1, ParserGen.gen1, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa ~ p2.fa
      val cres = composed.parse(str)

      val composed1 = FlatMap[Parser].product(p1.fa, p2.fa)
      val cres1 = composed1.parse(str)
      assertEquals(cres, cres1)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = Parser.length(off) *> p2.fa
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.with1 ~ b composes as expected") {
    forAll(ParserGen.gen, ParserGen.gen1, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.with1 ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = Parser.length(off) *> p2.fa
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.soft ~ b composes as expected") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser.Error(fidx, errs) if (fidx == off) => Parser.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a1.soft ~ b composes as expected") {
    forAll(ParserGen.gen1, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser.Error(fidx, errs) if (fidx == off) => Parser.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.with1.soft ~ b1 composes as expected") {
    forAll(ParserGen.gen, ParserGen.gen1, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.with1.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser.Error(fidx, errs) if (fidx == off) => Parser.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  test("range messages seem to work") {
    val pa = Parser.charIn('0' to '9')
    assertEquals(pa.parse("z").toString, "Left(Error(0,NonEmptyList(InRange(0,0,9))))")
  }

  test("partial parse fails in rep") {
    val partial = Parser.length1(1) ~ Parser.fail
    // we can't return empty list here
    assert(partial.rep.parse("foo").isLeft)

    val p2 = Parser.string1("f").orElse1((Parser.string1("boo") ~ Parser.string1("p")).void)
    assert(p2.rep1.parse("fboop").isRight)
    assert(p2.rep1(2).parse("fboop").isRight)
    assert(p2.rep1(3).parse("fboop").isLeft)
    assert(p2.rep1.parse("fboof").isLeft)
  }

  test("defer does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser].defer {
      cnt += 1
      Parser.string1("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }

  test("defer1 does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser1].defer {
      cnt += 1
      Parser.string1("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }

  property("charIn matches charWhere") {
    forAll { (cs: List[Char], str: String) =>
      val cset = cs.toSet
      val p1 = Parser.charIn(cs)
      val p2 = Parser.charWhere(cset)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("charIn matches charIn varargs") {
    forAll { (c0: Char, cs0: List[Char], str: String) =>
      val cs = c0 :: cs0
      val p1 = Parser.charIn(cs)
      val p2 = Parser.charIn(c0, cs0: _*)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("Parser.end gives the right error") {
    forAll { str: String =>
      Parser.end.parse(str) match {
        case Right((rest, _)) =>
          assertEquals(str, "")
          assertEquals(rest, "")
        case Left(Parser.Error(0, NonEmptyList(Parser.Expectation.EndOfString(off, len), Nil))) =>
          assertEquals(off, 0)
          assertEquals(len, str.length)
        case other =>
          fail(s"unexpected failure: $other")
      }
    }
  }

  property("rep can be reimplemented with oneOf and defer") {
    forAll(ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP, str) =>
      def rep[A](pa: Parser1[A]): Parser[List[A]] =
        Defer[Parser].fix[List[A]] { tail =>
          (pa ~ tail)
            .map { case (h, t) => h :: t }
            .orElse(Parser.pure(Nil))
        }

      val lst1 = rep(genP.fa)
      val lst2 = genP.fa.rep

      assertEquals(lst1.parse(str), lst2.parse(str))
    }
  }

  property("rep is consistent with rep1") {
    forAll(ParserGen.gen1, Gen.choose(0, Int.MaxValue), Arbitrary.arbitrary[String]) { (genP, min0, str) =>

      val min = min0 & Int.MaxValue
      val repA = genP.fa.rep(min)
      val repB = genP.fa.rep1(min).map(_.toList).orElse(
        if (min == 0) Parser.pure(Nil)
        else Parser.fail
      )

      assertEquals(repA.parse(str), repB.parse(str))
    }
  }

  property("repSep with unit sep is the same as rep") {
    forAll(ParserGen.gen1, Gen.choose(0, Int.MaxValue), Arbitrary.arbitrary[String]) { (genP, min0, str) =>
      val min = min0 & Int.MaxValue
      val p1a = Parser.repSep(genP.fa, min = min, sep = Parser.unit)
      val p1b = genP.fa.rep(min = min)

      assertEquals(p1a.parse(str), p1b.parse(str))

      val min1 = if (min < 1) 1 else min
      val p2a = Parser.rep1Sep(genP.fa, min = min1, sep = Parser.unit)
      val p2b = genP.fa.rep1(min = min1)

      assertEquals(p2a.parse(str), p2b.parse(str))
    }
  }

  property("rep1Sep with sep = fail is the same as parsing 1") {
    forAll(ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP, str) =>
      assertEquals(genP.fa.parse(str), Parser.rep1Sep(genP.fa, 1,
        Parser.fail).parse(str).map { case (rest, nel) => (rest, nel.head) })
    }
  }

  property("charsWhile/charWhile consistency") {
    forAll(Gen.choose(0, 100).flatMap(Gen.listOfN(_, Gen.choose(Char.MinValue, Char.MaxValue))),
      Arbitrary.arbitrary[String]) { (chars, str)=>

      val pred = chars.toSet
      val p1a = Parser.charsWhile(pred)
      val p1b = Parser.charWhere(pred).rep.string
      assertEquals(p1a.parse(str), p1b.parse(str))

      val p2a = Parser.charsWhile1(pred)
      val p2b = Parser.charWhere(pred).rep1.string
      assertEquals(p2a.parse(str), p2b.parse(str))
    }
  }

  property("MonoidK[Parser].empty never succeeds") {
    forAll { str: String =>
      assert(MonoidK[Parser].empty.parse(str).isLeft)
      assert(MonoidK[Parser1].empty.parse(str).isLeft)
    }
  }

  property("Monad.pure is an identity function") {
    forAll { (i: Int, str: String) =>
      assertEquals(Monad[Parser].pure(i).parse(str), Right((str, i)))
    }
  }

  property("p orElse p == p") {
    forAll(ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      val res1 = genP.fa.orElse(genP.fa).parse(str)
      assertEquals(res1, res0)
    }
  }

  property("p orElse1 p == p") {
    forAll(ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      val res1 = genP.fa.orElse1(genP.fa).parse(str)
      assertEquals(res1, res0)
    }
  }

  property("Parser1 fails or consumes 1 or more") {
    forAll(ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      res0 match {
        case Left(_) => assert(true)
        case Right((s, _)) => assert(str != s)
      }
    }
  }

  property("p1.backtrack.orElse(p2) succeeds if either p1 or p2 do") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val ores = p1.fa.backtrack.orElse(p2.fa).parse(str)
      val r1 = p1.fa.parse(str)
      val r = if (r1.isLeft) p2.fa.parse(str) else r1
      (ores, r) match {
        case (Left(_), l) => assert(l.isLeft)
        case (ra, rb) => assertEquals(ra, rb)
      }
    }
  }

  property("p1.backtrack.orElse1(p2) succeeds if either p1 or p2 do") {
    forAll(ParserGen.gen1, ParserGen.gen1, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val ores = p1.fa.backtrack.orElse1(p2.fa).parse(str)
      val r1 = p1.fa.parse(str)
      val r = if (r1.isLeft) p2.fa.parse(str) else r1
      (ores, r) match {
        case (Left(_), l) => assert(l.isLeft)
        case (ra, rb) => assertEquals(ra, rb)
      }
    }
  }

  test("charWhere(_ => true) == anyChar") {
    assertEquals(Parser.charWhere(_ => true), Parser.anyChar)
  }

  property("with1 *> and with1 <* work as expected") {
    forAll(ParserGen.gen, ParserGen.gen1, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val rp1 = p1.fa.with1 *> p2.fa
      val rp2 = (p1.fa.with1 ~ p2.fa).map(_._2)
      assertEquals(rp1.parse(str), rp2.parse(str))

      val rp3 = p1.fa.with1 <* p2.fa
      val rp4 = (p1.fa.with1 ~ p2.fa).map(_._1)
      assertEquals(rp3.parse(str), rp4.parse(str))
    }
  }

  property("a1 *> b and a1 <* b") {
    forAll(ParserGen.gen1, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      assertEquals((p1.fa *> p2.fa).parse(str), Parser.product10(p1.fa.void, p2.fa).map(_._2).parse(str))
      assertEquals((p1.fa <* p2.fa).parse(str), Parser.product10(p1.fa, p2.fa.void).map(_._1).parse(str))
    }
  }

  property("exactly one of x or !x parse") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, str) =>
      val notx = !p1.fa

      val xor = p1.fa.parse(str).isRight ^ notx.parse(str).isRight
      assert(xor)
    }
  }

  property("if x ~ y matches then x ~ y.peek match") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (x, y, str) =>
      val m1 = (x.fa ~ y.fa).parse(str)
      val m2 = ((x.fa ~ y.fa.peek).map(_._1)).parse(str)

      assertEquals(m1.isRight, m2.isRight)
      if (m1.isRight) {
        assert(x.fa.parse(str) == m2)
      }
    }
  }

  property("if x matches then x.peek matches but returns the whole string and unit") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (x, str) =>
      if (x.fa.parse(str).isRight) {
        assertEquals(x.fa.peek.parse(str), Right((str, ())))
      }
    }
  }

  property("(a.soft ~ b) == a ~ b in success of expected (not partials)") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left = a.fa.soft ~  b.fa
      val right = a.fa ~ b.fa
      val leftRes = left.parse(str).leftMap(_.expected)
      val rightRes = right.parse(str).leftMap(_.expected)
      assertEquals(leftRes, rightRes)
    }
  }

  property("(a.soft ~ b) == softProduct(a, b)") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left = a.fa.soft ~  b.fa
      val right = Parser.softProduct(a.fa, b.fa)
      assertEquals(left.parse(str), right.parse(str))
      assertEquals((a.fa.soft *> b.fa).parse(str), Parser.softProduct(a.fa.void, b.fa).map(_._2).parse(str))
      assertEquals((a.fa.soft <* b.fa).parse(str), Parser.softProduct(a.fa, b.fa.void).map(_._1).parse(str))
    }
  }

  property("(a1.soft ~ b) == softProduct10(a, b)") {
    forAll(ParserGen.gen1, ParserGen.gen, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left1 = a.fa.soft ~  b.fa
      val right1 = Parser.softProduct10(a.fa, b.fa)
      assertEquals(left1.parse(str), right1.parse(str))

      val left2 = b.fa.soft ~  a.fa
      val right2 = Parser.softProduct01(b.fa, a.fa)
      assertEquals(left2.parse(str), right2.parse(str))

      assertEquals((a.fa.soft *> b.fa).parse(str), Parser.softProduct10(a.fa.void, b.fa).map(_._2).parse(str))
      assertEquals((b.fa.with1.soft <* a.fa).parse(str), Parser.softProduct01(b.fa, a.fa.void).map(_._1).parse(str))
      assertEquals((b.fa.with1.soft *> a.fa).parse(str), Parser.softProduct01(b.fa.void, a.fa).map(_._2).parse(str))
    }
  }

  property("Parser.until is like a search") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val p = Parser.until(a.fa) *> a.fa
      def loopMatch(cnt: Int): Option[(String, a.A)] =
        (Parser.length(cnt) *> a.fa).parse(str) match {
          case Right(res) => Some(res)
          case Left(_) if cnt > str.length => None
          case _ => loopMatch(cnt + 1)
        }

      assertEquals(p.parse(str).toOption, loopMatch(0))
    }
  }

  property("parseAll law") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pall = (a.fa <* Parser.end).parse(str).map(_._2)

      assertEquals(a.fa.parseAll(str), pall)
    }
  }

  property("BitSetUtil union works") {
    forAll { cs: List[List[Char]] =>
      val arys = cs.filter(_.nonEmpty).map(_.toArray.sorted)
      val bs = arys.map { ary => (ary(0).toInt, BitSetUtil.bitSetFor(ary)) }
      val sortedFlat = BitSetUtil.union(bs)
      assertEquals(sortedFlat.toSet, cs.flatten.toSet)
    }
  }
}
