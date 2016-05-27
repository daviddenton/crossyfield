package io.github.daviddenton.crossyfield

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import org.scalatest._

class ValidatorsTest extends FunSpec with ShouldMatchers {

  describe("Validators") {
    val uuid = UUID.randomUUID()
    describe("string.required") {
      it("success") {
        Validators.string.required.int('id) <--? "1" shouldBe Validated(1)
        Validators.string.required.long('id) <--? "1" shouldBe Validated(1)
        Validators.string.required.double('id) <--? "1" shouldBe Validated(1.0d)
        Validators.string.required.float('id) <--? "1" shouldBe Validated(1.0f)
        (Validators.string.required.boolean('id) <--? "true") shouldBe Validated(true)
        Validators.string.required.char('id) <--? "1.2345" shouldBe Validated('1')
        Validators.string.required.localDate('id) <--? "2000-01-01" shouldBe Validated(LocalDate.of(2000, 1, 1))
        Validators.string.required.localDateTime('id) <--? "1970-01-01T00:00:00" shouldBe Validated(LocalDateTime.of(1970, 1, 1, 0, 0, 0, 0))
        (Validators.string.required.zonedDateTime('id) <--? "1970-01-01T00:00:00-00:00").toString shouldBe "Validated(1970-01-01T00:00Z)"
        Validators.string.required.bigDecimal('id) <--? "1.2345" shouldBe Validated(BigDecimal("1.2345"))
        Validators.string.required.uuid('id) <--? uuid.toString shouldBe Validated(uuid)
      }

      it("invalid") {
        Validators.string.required.int('id) <--? "" shouldBe Invalid('id -> "invalid int")
        Validators.string.required.long('id) <--? "" shouldBe Invalid('id -> "invalid long")
        Validators.string.required.double('id) <--? "" shouldBe Invalid('id -> "invalid double")
        Validators.string.required.float('id) <--? "" shouldBe Invalid('id -> "invalid float")
        Validators.string.required.boolean('id) <--? "" shouldBe Invalid('id -> "invalid boolean")
        Validators.string.required.char('id) <--? "" shouldBe Invalid('id -> "invalid char")
        Validators.string.required.localDate('id) <--? "" shouldBe Invalid('id -> "invalid localDate")
        Validators.string.required.localDateTime('id) <--? "" shouldBe Invalid('id -> "invalid localDateTime")
        Validators.string.required.zonedDateTime('id) <--? "" shouldBe Invalid('id -> "invalid zonedDateTime")
        Validators.string.required.bigDecimal('id) <--? "" shouldBe Invalid('id -> "invalid bigDecimal")
        Validators.string.required.uuid('id) <--? "" shouldBe Invalid('id -> "invalid uuid")
      }
    }

    describe("string.optional") {
      it("success") {
        Validators.string.optional.int('id) <--? "1" shouldBe Validated(1)
        Validators.string.optional.long('id) <--? "1" shouldBe Validated(1)
        Validators.string.optional.double('id) <--? "1" shouldBe Validated(1.0d)
        Validators.string.optional.float('id) <--? "1" shouldBe Validated(1.0f)
        (Validators.string.optional.boolean('id) <--? "true") shouldBe Validated(true)
        Validators.string.optional.char('id) <--? "1.2345" shouldBe Validated('1')
        Validators.string.optional.localDate('id) <--? "2000-01-01" shouldBe Validated(LocalDate.of(2000, 1, 1))
        Validators.string.optional.localDateTime('id) <--? "1970-01-01T00:00:00" shouldBe Validated(LocalDateTime.of(1970, 1, 1, 0, 0, 0, 0))
        (Validators.string.optional.zonedDateTime('id) <--? "1970-01-01T00:00:00-00:00").toString shouldBe "Validated(1970-01-01T00:00Z)"
        Validators.string.optional.bigDecimal('id) <--? "1.2345" shouldBe Validated(BigDecimal("1.2345"))
        Validators.string.optional.uuid('id) <--? uuid.toString shouldBe Validated(uuid)
      }

      it("missing") {
        Validators.string.optional.int('id) <--? "" shouldBe Ignored
        Validators.string.optional.long('id) <--? "" shouldBe Ignored
        Validators.string.optional.double('id) <--? "" shouldBe Ignored
        Validators.string.optional.float('id) <--? "" shouldBe Ignored
        Validators.string.optional.boolean('id) <--? "" shouldBe Ignored
        Validators.string.optional.char('id) <--? "" shouldBe Ignored
        Validators.string.optional.localDate('id) <--? "" shouldBe Ignored
        Validators.string.optional.localDateTime('id) <--? "" shouldBe Ignored
        Validators.string.optional.zonedDateTime('id) <--? "" shouldBe Ignored
        Validators.string.optional.bigDecimal('id) <--? "" shouldBe Ignored
        Validators.string.optional.uuid('id) <--? "" shouldBe Ignored
      }
    }
  }
}
