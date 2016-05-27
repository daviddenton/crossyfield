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
    dateValidator('startDate) <--?("2000-01-01", "after the millenium", _.isAfter(millennium)),
    dateValidator('middleDate) <--?("1999-01-01", "after the millenium", _.isAfter(millennium)),
    dateValidator('startDate) <--?("2003-01-01", "after the millenium", _.isAfter(millennium))
  )

  println("Erroneous fields: " + errors.mkString(", "))
}
