package io.github.daviddenton.crossyfield

import org.scalatest._

class ValidatorTest extends FunSpec with ShouldMatchers {

  describe("validator") {
    it("all Successful") {
      Validator.mk(
        Successful(1),
        Successful(2)
      ) { case (first, second) => (first, second) } shouldBe Successful((Some(1), Some(2)))
    }

    it("all Successful or NotProvided") {
      Validator.mk(
        Successful(1),
        NotProvided
      ) { case (first, second) => (first, second) } shouldBe Successful((Some(1), None))
    }
    it("collects Errors") {
      Validator.mk(
        Errors('ex1 -> "invalid1"),
        Errors('ex2 -> "invalid2")
      ) { case (first, second) => (first, second) } shouldBe Errors(List(('ex1,"invalid1"), ('ex2,"invalid2")))
    }
  }
}
