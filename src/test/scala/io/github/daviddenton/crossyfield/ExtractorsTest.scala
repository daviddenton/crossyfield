package io.github.daviddenton.crossyfield

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import org.scalatest._

class ExtractorsTest extends FunSpec with ShouldMatchers {

  describe("Extractors") {
    val uuid = UUID.randomUUID()
    describe("string.required") {
      it("success") {
        Extractors.string.required.int('id) <--? "1" shouldBe Extracted(1)
        Extractors.string.required.long('id) <--? "1" shouldBe Extracted(1)
        Extractors.string.required.double('id) <--? "1" shouldBe Extracted(1.0d)
        Extractors.string.required.float('id) <--? "1" shouldBe Extracted(1.0f)
        (Extractors.string.required.boolean('id) <--? "true") shouldBe Extracted(true)
        Extractors.string.required.char('id) <--? "1.2345" shouldBe Extracted('1')
        Extractors.string.required.localDate('id) <--? "2000-01-01" shouldBe Extracted(LocalDate.of(2000, 1, 1))
        Extractors.string.required.localDateTime('id) <--? "1970-01-01T00:00:00" shouldBe Extracted(LocalDateTime.of(1970, 1, 1, 0, 0, 0, 0))
        (Extractors.string.required.zonedDateTime('id) <--? "1970-01-01T00:00:00-00:00").toString shouldBe "Extracted(1970-01-01T00:00Z)"
        Extractors.string.required.bigDecimal('id) <--? "1.2345" shouldBe Extracted(BigDecimal("1.2345"))
        Extractors.string.required.uuid('id) <--? uuid.toString shouldBe Extracted(uuid)
      }

      it("invalid") {
        Extractors.string.required.int('id) <--? "" shouldBe Invalid('id -> "invalid int")
        Extractors.string.required.long('id) <--? "" shouldBe Invalid('id -> "invalid long")
        Extractors.string.required.double('id) <--? "" shouldBe Invalid('id -> "invalid double")
        Extractors.string.required.float('id) <--? "" shouldBe Invalid('id -> "invalid float")
        Extractors.string.required.boolean('id) <--? "" shouldBe Invalid('id -> "invalid boolean")
        Extractors.string.required.char('id) <--? "" shouldBe Invalid('id -> "invalid char")
        Extractors.string.required.localDate('id) <--? "" shouldBe Invalid('id -> "invalid localDate")
        Extractors.string.required.localDateTime('id) <--? "" shouldBe Invalid('id -> "invalid localDateTime")
        Extractors.string.required.zonedDateTime('id) <--? "" shouldBe Invalid('id -> "invalid zonedDateTime")
        Extractors.string.required.bigDecimal('id) <--? "" shouldBe Invalid('id -> "invalid bigDecimal")
        Extractors.string.required.uuid('id) <--? "" shouldBe Invalid('id -> "invalid uuid")
      }
    }

    describe("string.optional") {
      it("success") {
        Extractors.string.optional.int('id) <--? "1" shouldBe Extracted(1)
        Extractors.string.optional.long('id) <--? "1" shouldBe Extracted(1)
        Extractors.string.optional.double('id) <--? "1" shouldBe Extracted(1.0d)
        Extractors.string.optional.float('id) <--? "1" shouldBe Extracted(1.0f)
        (Extractors.string.optional.boolean('id) <--? "true") shouldBe Extracted(true)
        Extractors.string.optional.char('id) <--? "1.2345" shouldBe Extracted('1')
        Extractors.string.optional.localDate('id) <--? "2000-01-01" shouldBe Extracted(LocalDate.of(2000, 1, 1))
        Extractors.string.optional.localDateTime('id) <--? "1970-01-01T00:00:00" shouldBe Extracted(LocalDateTime.of(1970, 1, 1, 0, 0, 0, 0))
        (Extractors.string.optional.zonedDateTime('id) <--? "1970-01-01T00:00:00-00:00").toString shouldBe "Extracted(1970-01-01T00:00Z)"
        Extractors.string.optional.bigDecimal('id) <--? "1.2345" shouldBe Extracted(BigDecimal("1.2345"))
        Extractors.string.optional.uuid('id) <--? uuid.toString shouldBe Extracted(uuid)
      }

      it("missing") {
        Extractors.string.optional.int('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.long('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.double('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.float('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.boolean('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.char('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.localDate('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.localDateTime('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.zonedDateTime('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.bigDecimal('id) <--? "" shouldBe NotProvided
        Extractors.string.optional.uuid('id) <--? "" shouldBe NotProvided
      }
    }
  }
}
