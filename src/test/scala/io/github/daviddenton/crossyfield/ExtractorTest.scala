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
          c <- Successful(1)
        } yield Inner(a, c.get)
      }

      val extractOuter = Extractor.mk('outer) {
        s: String => for {
          inner <- extractInner <--? s
          e <- Successful(true)
        } yield Outer(inner, e.get)
      }

      extractOuter <--? "" shouldBe Successful(Outer(Some(Inner(None, 1)), true))
    }

    it("does not short circuit if all lines in a for comprehension are NotProvided") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- NotProvided
          second <- NotProvided
        } yield (first, second)
      }
      ex <--? "" shouldBe Successful((None, None))
    }

    it("does not short circuit if last line in a for comprehension is optional") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Successful(123)
          second <- NotProvided
        } yield (first, second)
      }
      ex <--? "" shouldBe Successful((Some(123), None))
    }

    it("when all are extracted") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Successful(123)
          second <- Successful(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Successful((Some(123), Some(456)))
      ex extract "" shouldBe Successful((Some(123), Some(456)))
    }

    it("invalid when first is invalid") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Errors('first, "reason")
          second <- Successful(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Errors('first, "reason")
    }

    it("only reports the first failure") {
      val ex = Extractor.mk('ex) {
        s: String => for {
          first <- Errors('first, "reason")
          second <- Errors('second, "reason2")
        } yield (first, second)
      }
      ex <--? "" shouldBe Errors('first, "reason")
    }

    it("handles cross field extraction failure") {
      val ex = Extractor.mk('ex, "a reason", (s: String) => s)
      ex <--?("bob", "reason", _ != "bob") shouldBe Errors('ex, "reason")
      ex extract("bob", "reason", _ != "bob") shouldBe Errors('ex, "reason")
    }

    it("handles cross field extraction success") {
      val ex = Extractor.mk('ex, "a reason", (s: String) => s)
      ex <--?("bob", "reason", _ == "bob") shouldBe Successful("bob")
    }

    it("extraction failure - function throws") {
      val ex = Extractor.mk('ex, "a reason", (s: String) => throw new RuntimeException())
      ex <--?("bob", "reason", _ == "bob") shouldBe Errors('ex, "a reason")
    }

    describe("falling back to default value") {
      it("Extracted") {
        Successful(true).orDefault(false) shouldBe Successful(true)
      }
      it("NotProvided") {
        NotProvided.orDefault(true) shouldBe Successful(true)
      }
      it("ExtractionFailed") {
        Errors('ex, "name").orDefault(true) shouldBe Errors('ex, "name")
      }
    }

    describe("misc methods") {
      it("toString") {
        Successful(1).toString shouldBe "Successful(1)"
        NotProvided.toString shouldBe "NotProvided"
        Errors(Seq('ex -> "invalid", 'ex -> "missing")).toString shouldBe "Errors(List(('ex,invalid), ('ex,missing)))"
      }
      it("map") {
        Successful(1).map(_ => 1) shouldBe Successful(1)
        NotProvided.map(_ => 1) shouldBe Successful(1)
        Errors(Seq('ex -> "invalid", 'ex -> "missing")).map(_ => 1) shouldBe Errors(Seq('ex -> "invalid", 'ex -> "missing"))
      }
//      it("collectErrors") {
//        Extraction.collectErrors() shouldBe Nil
//        Extraction.collectErrors(NotProvided, Extracted(Some(1), Extracted(None))) shouldBe Nil
//        Extraction.collectErrors(Invalid('ex -> "invalid")) shouldBe Seq('ex -> "invalid")
//      }
      it("flatten") {
        Extraction.flatten(NotProvided) shouldBe NotProvided
        Extraction.flatten(Successful(None)) shouldBe NotProvided
        Extraction.flatten(Successful(Some(1))) shouldBe Successful(1)
        Extraction.flatten(Errors('ex -> "invalid")) shouldBe Errors('ex -> "invalid")
      }
      it("<--?") {
        Extraction.<--?(Seq(NotProvided, NotProvided)) shouldBe NotProvided
        Extraction.<--?(Seq(Successful(1), Successful(1))) shouldBe NotProvided
        Extraction.<--?(Seq(NotProvided, Successful(1), Errors('ex -> "missing"), Errors('ex -> "invalid"))) shouldBe Errors(Seq('ex -> "missing", 'ex -> "invalid"))
      }
    }
  }
}
