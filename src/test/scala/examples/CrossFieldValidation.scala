package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Errors, Extractor, Extractors}

import scala.util.{Success, Try}

object CrossFieldValidation extends App {

  /**
    * Simple case class which represents an item we wish to extract and validate
    */
  case class Range(startDate: LocalDate, middleDate: Option[LocalDate], endDate: LocalDate)

  /**
    * Provides an Extractor that checks there is a valid date string at the specified index in the CSV string
    */
  def dateExtractor(id: Symbol, index: Int, required: Boolean): Extractor[String, LocalDate] = Extractor.mk(id) {
    in: String =>
      Try(in.split(",")(index)) match {
        case Success(dateStr) if required => Extractors.string.required.localDate(id) <--? dateStr
        case Success(dateStr) => Extractors.string.optional.localDate(id) <--? dateStr
        case _ => Errors(id -> s"Missing date at index $index")
      }
  }

  val startDate = dateExtractor('startDate, 0, required = true)
  val middleDate = dateExtractor('middleDate, 1, required = false)
  val endDate = dateExtractor('endDate, 2, required = true)

  /**
    * This composite Extractor shows has other Extractors embedded in it's logic. You can cross-extract the result
    */
  val rangeExtraction = Extractor.mk('range) {
    input: String => {
      for {
        startDate <- startDate <--? input
        middleDate <- middleDate <--?(input, "middle date not after start", _.isAfter(startDate.get))
        endDate <- endDate <--?(input, "end date not after start", e => startDate.map(s => e.isAfter(s)).getOrElse(true))
      } yield Range(startDate.get, middleDate, endDate.get)
    }
  }

  println("Empty string: ", rangeExtraction <--? "")
  println("Fully specified range: ", rangeExtraction <--? "2000-01-01,2001-01-01,2002-01-01")
  println("Missing middle date: ", rangeExtraction <--? "2000-01-01,,2002-01-01")
  println("Middle date is before start: ", rangeExtraction <--? "2001-01-01,2000-01-01,2002-01-01")
  println("End date is before start: ", rangeExtraction <--? "2001-01-01,2002-01-01,2000-01-01")
  println("Perform further extraction on the output object of the range", rangeExtraction <--?("2000-01-01,2001-01-01,2002-01-01", "range is too new", _.startDate.isBefore(LocalDate.of(1990, 1, 1))))

}
