package io.github.daviddenton.crossyfield

import org.scalatest._

class ValidatorTest extends FunSpec with ShouldMatchers {

  describe("validator") {
    it("all Successful") {
      Validator.mk(
        Extracted(1),
        Extracted(2)
      ) { case (first, second) => (first, second) } shouldBe Validated((Some(1), Some(2)))
    }

    it("all Successful or NotProvided") {
      Validator.mk(
        Extracted(1),
        NotProvided
      ) { case (first, second) => (first, second) } shouldBe Validated((Some(1), None))
    }

    it("all NotProvided") {
      Validator.mk(
        NotProvided,
        NotProvided
      ) { case (first, second) => (first, second) } shouldBe Validated((None, None))
    }

    it("collects Errors") {
      Validator.mk(
        ExtractionFailed('ex1 -> "invalid1"),
        ExtractionFailed('ex2 -> "invalid2")
      ) { case (first, second) => (first, second) } shouldBe ValidationFailed(List(('ex1,"invalid1"), ('ex2,"invalid2")))
    }
  }
}
