package io.github.daviddenton.crossyfield

import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.util.UUID

class PrimitiveValidators(required: Boolean) {
  private def mk[T](id: Symbol, msg: String, required: Boolean, fn: String => T) =
    Validator.mk(id) {
      in: String => if (in.isEmpty && !required) Ignored else Validator.mk(id, msg, fn) <--? in
    }

  def int(id: Symbol, msg: String = "invalid int") = mk(id, msg, required, (s: String) => s.toInt)

  def double(id: Symbol, msg: String = "invalid double") = mk(id, msg, required, (s: String) => s.toDouble)

  def long(id: Symbol, msg: String = "invalid long") = mk(id, msg, required, (s: String) => s.toLong)

  def float(id: Symbol, msg: String = "invalid float") = mk(id, msg, required, (s: String) => s.toFloat)

  def uuid(id: Symbol, msg: String = "invalid uuid") = mk(id, msg, required, UUID.fromString)

  def bigDecimal(id: Symbol, msg: String = "invalid bigDecimal") = mk(id, msg, required, (s: String) => BigDecimal(s))

  def boolean(id: Symbol, msg: String = "invalid boolean") = mk(id, msg, required, (s: String) => s.toBoolean)

  def char(id: Symbol, msg: String = "invalid char") = mk(id, msg, required, (s: String) => s.charAt(0))

  def localDateTime(id: Symbol, msg: String = "invalid localDateTime") = mk(id, msg, required, LocalDateTime.parse)

  def localDate(id: Symbol, msg: String = "invalid localDate") = mk(id, msg, required, LocalDate.parse)

  def zonedDateTime(id: Symbol, msg: String = "invalid zonedDateTime") = mk(id, msg, required, ZonedDateTime.parse)

}
