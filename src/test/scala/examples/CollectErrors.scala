package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Extraction, Extractors}

object CollectErrors extends App {

  /**
    * Provides an optional Extractor using the convenience methods
    */
  def dateExtractor(id: Symbol) = Extractors.string.optional.localDate(id)

  val millennium = LocalDate.of(2000, 1, 1)

  /**
    * Because we are interested in collecting ALL of the errors, we can't use cross-field extraction here
    */
  val errors = Extraction.collectErrors(
    dateExtractor('theFuture) <--?("2000-01-01", "must be after the millennium", _.isAfter(millennium)),
    dateExtractor('anyOldDate) <--? "NOTADATE-01-01",
    dateExtractor('thePast) <--?("2003-01-01", "must be before the millennium", _.isBefore(millennium))
  )

  println("Erroneous fields: " + errors.mkString(", "))
}
