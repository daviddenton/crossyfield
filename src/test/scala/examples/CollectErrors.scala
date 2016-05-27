package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Ignored, Validation, Validator}

object CollectErrors extends App {

  /**
    * Provides an Optional validator
    */
  def dateValidator(id: Symbol) = Validator.mk(id) {
    in: String =>
      if (in.isEmpty) Ignored
      else Validator.mk(id, "invalid date", LocalDate.parse) <--? in
  }

  val millennium = LocalDate.of(2000, 1, 1)

  val errors = Validation.collectErrors(
    dateValidator('theFuture) <--?("2000-01-01", "must be after the millennium", _.isAfter(millennium)),
    dateValidator('anyOldDate) <--? "ASDA-01-01",
    dateValidator('thePast) <--?("2003-01-01", "must be before the millennium", _.isBefore(millennium))
  )

  println("Erroneous fields: " + errors.mkString(", "))
}
