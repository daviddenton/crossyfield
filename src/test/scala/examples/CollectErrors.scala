package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Validation, Validators}

object CollectErrors extends App {

  /**
    * Provides an optional validator using the convenience methods
    */
  def dateValidator(id: Symbol) = Validators.string.optional.localDate(id)

  val millennium = LocalDate.of(2000, 1, 1)

  /**
    * Because we are interested in collecting ALL of the errors, we can't use cross-field validation here
    */
  val errors = Validation.collectErrors(
    dateValidator('theFuture) <--?("2000-01-01", "must be after the millennium", _.isAfter(millennium)),
    dateValidator('anyOldDate) <--? "ASDA-01-01",
    dateValidator('thePast) <--?("2003-01-01", "must be before the millennium", _.isBefore(millennium))
  )

  println("Erroneous fields: " + errors.mkString(", "))
}
