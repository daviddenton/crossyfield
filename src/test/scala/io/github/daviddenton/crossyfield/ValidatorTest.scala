package io.github.daviddenton.crossyfield

import org.scalatest._

class ValidatorTest extends FunSpec with ShouldMatchers {

  case class Inner(a: Option[Int], c: Int)

  case class Outer(d: Option[Inner], e: Boolean)

  describe("Validatable") {

    it("embedded extraction") {

      val validateInner = Validator.mk {
        s: String => for {
          a <- Ignored
          c <- Validated(1)
        } yield Inner(a, c.get)
      }

      val validateOuter = Validator.mk {
        s: String => for {
          inner <- validateInner <--? s
          e <- Validated(true)
        } yield Outer(inner, e.get)
      }

      validateOuter <--? "" shouldBe Validated(Outer(Some(Inner(None, 1)), true))
    }

    it("does not short circuit if all lines in a for comprehension are Ignored") {
      val ex = Validator.mk {
        s: String => for {
          first <- Ignored
          second <- Ignored
        } yield (first, second)
      }
      ex <--? "" shouldBe Validated((None, None))
    }

    it("does not short circuit if last line in a for comprehension is optional") {
      val ex = Validator.mk {
        s: String => for {
          first <- Validated(123)
          second <- Ignored
        } yield (first, second)
      }
      ex <--? "" shouldBe Validated((Some(123), None))
    }

    it("when all are validated") {
      val ex = Validator.mk {
        s: String => for {
          first <- Validated(123)
          second <- Validated(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Validated((Some(123), Some(456)))
    }

    it("invalid when first is invalid") {
      val ex = Validator.mk {
        s: String => for {
          first <- Invalid("reason")
          second <- Validated(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Invalid("reason")
    }

    it("only reports the first failure") {
      val ex = Validator.mk {
        s: String => for {
          first <- Invalid("reason")
          second <- Invalid("reason2")
        } yield (first, second)
      }
      ex <--? "" shouldBe Invalid("reason")
    }

    it("handles cross field validation failure") {
      val ex = Validator.mk { s: String => Validated(s) }
      ex <--?("bob", "reason", _ != "bob") shouldBe Invalid("reason")
    }

    it("handles cross field validation success") {
      val ex = Validator.mk { s: String => Validated(s) }
      ex <--?("bob", "reason", _ == "bob") shouldBe Validated("bob")
    }

    describe("falling back to default value") {
      it("Validated") {
        Validated(true).orDefault(false) shouldBe Validated(true)
      }
      it("Ignored") {
        Ignored.orDefault(true) shouldBe Validated(true)
      }
      it("ValidationFailed") {
        Invalid("name").orDefault(true) shouldBe Invalid("name")
      }
    }

    describe("misc methods") {
      it("flatten") {
        Validation.flatten(Ignored) shouldBe Ignored
        Validation.flatten(Validated(None)) shouldBe Ignored
        Validation.flatten(Validated(Some(1))) shouldBe Validated(1)
        Validation.flatten(Invalid(Seq("invalid"))) shouldBe Invalid(Seq("invalid"))
      }
      it("combine") {
        Validation.combine(Seq(Ignored, Ignored)) shouldBe Ignored
        Validation.combine(Seq(Ignored, Validated(1))) shouldBe Ignored
        Validation.combine(Seq(Ignored, Validated(1), Invalid("missing"), Invalid("invalid"))) shouldBe Invalid(Seq("missing", "invalid"))
      }
    }
  }
}
