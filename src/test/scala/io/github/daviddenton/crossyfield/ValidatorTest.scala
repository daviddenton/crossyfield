package io.github.daviddenton.crossyfield

import org.scalatest._

class ValidatorTest extends FunSpec with ShouldMatchers {

  case class Inner(a: Option[Int], c: Int)

  case class Outer(d: Option[Inner], e: Boolean)

  describe("Validatable") {

    it("embedded extraction") {

      val validateInner = Validator.mk('inner) {
        s: String => for {
          a <- Ignored
          c <- Validated(1)
        } yield Inner(a, c.get)
      }

      val validateOuter = Validator.mk('outer) {
        s: String => for {
          inner <- validateInner <--? s
          e <- Validated(true)
        } yield Outer(inner, e.get)
      }

      validateOuter <--? "" shouldBe Validated(Outer(Some(Inner(None, 1)), true))
    }

    it("does not short circuit if all lines in a for comprehension are Ignored") {
      val ex = Validator.mk('ex) {
        s: String => for {
          first <- Ignored
          second <- Ignored
        } yield (first, second)
      }
      ex <--? "" shouldBe Validated((None, None))
    }

    it("does not short circuit if last line in a for comprehension is optional") {
      val ex = Validator.mk('ex) {
        s: String => for {
          first <- Validated(123)
          second <- Ignored
        } yield (first, second)
      }
      ex <--? "" shouldBe Validated((Some(123), None))
    }

    it("when all are validated") {
      val ex = Validator.mk('ex) {
        s: String => for {
          first <- Validated(123)
          second <- Validated(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Validated((Some(123), Some(456)))
      ex validate "" shouldBe Validated((Some(123), Some(456)))
    }

    it("invalid when first is invalid") {
      val ex = Validator.mk('ex) {
        s: String => for {
          first <- Invalid('first, "reason")
          second <- Validated(456)
        } yield (first, second)
      }
      ex <--? "" shouldBe Invalid('first, "reason")
    }

    it("only reports the first failure") {
      val ex = Validator.mk('ex) {
        s: String => for {
          first <- Invalid('first, "reason")
          second <- Invalid('second, "reason2")
        } yield (first, second)
      }
      ex <--? "" shouldBe Invalid('first, "reason")
    }

    it("handles cross field validation failure") {
      val ex = Validator.mk('ex, "a reason" , (s: String) => s )
      ex <--?("bob", "reason", _ != "bob") shouldBe Invalid('ex, "reason")
      ex validate("bob", "reason", _ != "bob") shouldBe Invalid('ex, "reason")
    }

    it("handles cross field validation success") {
      val ex = Validator.mk('ex, "a reason" , (s: String) => s )
      ex <--?("bob", "reason", _ == "bob") shouldBe Validated("bob")
    }

    it("validation failure - function throws") {
      val ex = Validator.mk('ex, "a reason" , (s: String) => throw new RuntimeException() )
      ex <--?("bob", "reason", _ == "bob") shouldBe Invalid('ex, "a reason")
    }

    describe("falling back to default value") {
      it("Validated") {
        Validated(true).orDefault(false) shouldBe Validated(true)
      }
      it("Ignored") {
        Ignored.orDefault(true) shouldBe Validated(true)
      }
      it("ValidationFailed") {
        Invalid('ex, "name").orDefault(true) shouldBe Invalid('ex, "name")
      }
    }

    describe("misc methods") {
      it("toString") {
        Validated(1).toString shouldBe "Validated(1)"
        Ignored.toString shouldBe "Ignored"
        Invalid(Seq('ex -> "invalid", 'ex -> "missing")).toString shouldBe "Invalid(List(('ex,invalid), ('ex,missing)))"
      }
      it("map") {
        Validated(1).map(_ => 1) shouldBe Validated(1)
        Ignored.map(_ => 1) shouldBe Validated(1)
        Invalid(Seq('ex -> "invalid", 'ex -> "missing")).map(_ => 1) shouldBe Invalid(Seq('ex -> "invalid", 'ex -> "missing"))
      }
      it("collectErrors") {
        Validation.collectErrors() shouldBe Nil
        Validation.collectErrors(Ignored, Validated(Some(1), Validated(None))) shouldBe Nil
        Validation.collectErrors(Invalid('ex -> "invalid")) shouldBe Seq('ex -> "invalid")
      }
      it("flatten") {
        Validation.flatten(Ignored) shouldBe Ignored
        Validation.flatten(Validated(None)) shouldBe Ignored
        Validation.flatten(Validated(Some(1))) shouldBe Validated(1)
        Validation.flatten(Invalid('ex -> "invalid")) shouldBe Invalid('ex -> "invalid")
      }
      it("<--?") {
        Validation.<--?(Seq(Ignored, Ignored)) shouldBe Ignored
        Validation.<--?(Seq(Validated(1), Validated(1))) shouldBe Ignored
        Validation.<--?(Seq(Ignored, Validated(1), Invalid('ex -> "missing"), Invalid('ex -> "invalid"))) shouldBe Invalid(Seq('ex -> "missing", 'ex -> "invalid"))
      }
    }
  }
}
