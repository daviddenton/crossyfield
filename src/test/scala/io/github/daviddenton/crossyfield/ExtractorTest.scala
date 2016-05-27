package io.github.daviddenton.crossyfield

import org.scalatest._

class ExtractorTest extends FunSpec with ShouldMatchers {

  case class Inner(a: Option[Int], c: Int)

  case class Outer(d: Option[Inner], e: Boolean)

  describe("Extractable") {

    it("embedded extraction") {

      val extractInner = Extractor.mk('inner) {
        s: String => for {
          a <- NotProvided
          c <- Extracted(1)
        } yield Inner(a, c.get)
      }

      val extractOuter = Extractor.mk('outer) {
        s: String => for {
          inner <- extractInner <--? s
          e <- Extracted(true)
        } yield Outer(inner, e.get)
      }

      extractOuter <--? "" shouldBe Extracted(Outer(Some(Inner(None, 1)), true))
    }

    it("does not short circuit if all lines in a for comprehension are NotProvided") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- NotProvided
          second <- NotProvided
        } yield (first, second)
      }
      ex <--? "" shouldBe Extracted((None, None))
    }

    it("does not short circuit if last line in a for comprehension is optional") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Extracted(123)
          second <- NotProvided
        } yield (first, second)
      }
      ex <--? "" shouldBe Extracted((Some(123), None))
    }

    it("when all are extracted") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Extracted(123)
          second <- Extracted(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Extracted((Some(123), Some(456)))
      ex extract "" shouldBe Extracted((Some(123), Some(456)))
    }

    it("invalid when first is invalid") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Invalid('first, "reason")
          second <- Extracted(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Invalid('first, "reason")
    }

    it("only reports the first failure") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Invalid('first, "reason")
          second <- Invalid('second, "reason2")
        } yield (first, second)
      }
      ex <--? "" shouldBe Invalid('first, "reason")
    }

    it("handles cross field extraction failure") {
      val ex = Extractor.mk('ex, "a reason", (s: String) => s)
      ex <--?("bob", "reason", _ != "bob") shouldBe Invalid('ex, "reason")
      ex extract("bob", "reason", _ != "bob") shouldBe Invalid('ex, "reason")
    }

    it("handles cross field extraction success") {
      val ex = Extractor.mk('ex, "a reason", (s: String) => s)
      ex <--?("bob", "reason", _ == "bob") shouldBe Extracted("bob")
    }

    it("extraction failure - function throws") {
      val ex = Extractor.mk('ex, "a reason", (s: String) => throw new RuntimeException())
      ex <--?("bob", "reason", _ == "bob") shouldBe Invalid('ex, "a reason")
    }

    describe("falling back to default value") {
      it("Extracted") {
        Extracted(true).orDefault(false) shouldBe Extracted(true)
      }
      it("NotProvided") {
        NotProvided.orDefault(true) shouldBe Extracted(true)
      }
      it("ExtractionFailed") {
        Invalid('ex, "name").orDefault(true) shouldBe Invalid('ex, "name")
      }
    }

    describe("misc methods") {
      it("toString") {
        Extracted(1).toString shouldBe "Extracted(1)"
        NotProvided.toString shouldBe "NotProvided"
        Invalid(Seq('ex -> "invalid", 'ex -> "missing")).toString shouldBe "Invalid(List(('ex,invalid), ('ex,missing)))"
      }
      it("map") {
        Extracted(1).map(_ => 1) shouldBe Extracted(1)
        NotProvided.map(_ => 1) shouldBe Extracted(1)
        Invalid(Seq('ex -> "invalid", 'ex -> "missing")).map(_ => 1) shouldBe Invalid(Seq('ex -> "invalid", 'ex -> "missing"))
      }
      it("collectErrors") {
        Extraction.collectErrors() shouldBe Nil
        Extraction.collectErrors(NotProvided, Extracted(Some(1), Extracted(None))) shouldBe Nil
        Extraction.collectErrors(Invalid('ex -> "invalid")) shouldBe Seq('ex -> "invalid")
      }
      it("flatten") {
        Extraction.flatten(NotProvided) shouldBe NotProvided
        Extraction.flatten(Extracted(None)) shouldBe NotProvided
        Extraction.flatten(Extracted(Some(1))) shouldBe Extracted(1)
        Extraction.flatten(Invalid('ex -> "invalid")) shouldBe Invalid('ex -> "invalid")
      }
      it("<--?") {
        Extraction.<--?(Seq(NotProvided, NotProvided)) shouldBe NotProvided
        Extraction.<--?(Seq(Extracted(1), Extracted(1))) shouldBe NotProvided
        Extraction.<--?(Seq(NotProvided, Extracted(1), Invalid('ex -> "missing"), Invalid('ex -> "invalid"))) shouldBe Invalid(Seq('ex -> "missing", 'ex -> "invalid"))
      }
    }
  }
}
