package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Ignored, Validation, Validator}

object CollectErrors extends App {

  /**
    * Simple case class which represents an item we wish to validate
    */
  case class Range(startDate: LocalDate, middleDate: Option[LocalDate], endDate: LocalDate)

  /**
    * This validator checks that the string parses to a date
    */
  def dateValidator(identifier: Symbol) = Validator.mk(identifier) {
    in: String =>
      if (in.isEmpty) Ignored
      else Validator.mk(identifier, "invalid date", LocalDate.parse) <--? in
  }

  val millennium = LocalDate.of(2000, 1, 1)

  val errors = Validation.collectErrors(
    dateValidator('startDate) <--?("2000-01-01", "after the millenium", _.isAfter(millennium)),
    dateValidator('middleDate) <--?("1999-01-01", "after the millenium", _.isAfter(millennium)),
    dateValidator('startDate) <--?("2003-01-01", "after the millenium", _.isAfter(millennium))
  )

  println("Erroneous fields: " + errors.mkString(", "))
}
