package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Extractors, Validator}

object CollectErrors extends App {

  /**
    * Provides an optional Extractor using the convenience methods
    */
  def dateExtractor(id: Symbol) = Extractors.string.optional.localDate(id)

  val millennium = LocalDate.of(2000, 1, 1)

  case class SomeDateStrings(futureDate: String, oldDate: String, pastDate: String)
  /**
    * Because we are interested in collecting ALL of the errors, we can't use cross-field extraction here
    * - use a Validation instead
    */
  def validate(input: SomeDateStrings) = Validator.mk(
    dateExtractor('theFuture) <--?(input.futureDate, "must be after the millennium", _.isAfter(millennium)),
    dateExtractor('anyOldDate) <--? input.oldDate,
    dateExtractor('thePast) <--?(input.pastDate, "must be before the millennium", _.isBefore(millennium))
  ) {
    case (a, b, c) => s"validated ok: $a, $b, $c"
  }

  println(validate(SomeDateStrings("2010-01-01", "2000-01-01", "1999-01-01")))

  println(validate(SomeDateStrings("2000-01-01", "NOTADATE-01-01", "2003-01-01")))
}
