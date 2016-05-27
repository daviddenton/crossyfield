package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Ignored, Invalid, Validator}

import scala.util.{Success, Try}

object CrossFieldValidation extends App {

  /**
    * Simple case class which represents an item we wish to validate
    */
  case class Range(startDate: LocalDate, middleDate: Option[LocalDate], endDate: LocalDate)

  /**
    * This validator checks that there is a valid date string at the specified index in the CSV string
    */
  def dateValidator(identifier: Symbol, index: Int, required: Boolean) = Validator.mk(identifier) {
    in: String =>
      Try(in.split(",")(index)) match {
        case Success(dateStr) if !dateStr.isEmpty => Validator.mk(identifier, "invalid date", LocalDate.parse) <--? dateStr
        case Success(dateStr) if !required => Ignored
        case _ => Invalid(identifier -> s"Missing date at index $index")
      }
  }

  val startDate = dateValidator('startDate, 0, required = true)
  val middleDate = dateValidator('middleDate, 1, required = false)
  val endDate = dateValidator('endDate, 2, required = true)

  /**
    * This composite Validator shows has other Validators embedded in it's logic. You can cross validate the result
    * of any
    */
  val rangeValidation = Validator.mk('range) {
    input: String => {
      for {
        startDate <- startDate <--? input
        middleDate <- middleDate <--?(input, "middle date not after start", _.isAfter(startDate.get))
        endDate <- endDate <--?(input, "end date not after start", e => startDate.map(s => e.isAfter(s)).getOrElse(true))
      } yield Range(startDate.get, middleDate, endDate.get)
    }
  }

  println("Empty string: ", rangeValidation <--? "")
  println("Fully specified range: ", rangeValidation <--? "2000-01-01,2001-01-01,2002-01-01")
  println("Missing middle date: ", rangeValidation <--? "2000-01-01,,2002-01-01")
  println("Middle date is before start: ", rangeValidation <--? "2001-01-01,2000-01-01,2002-01-01")
  println("End date is before start: ", rangeValidation <--? "2001-01-01,2002-01-01,2000-01-01")
  println("Perform further validation on the output object of the range", rangeValidation <--?("2000-01-01,2001-01-01,2002-01-01", "range is too new", _.startDate.isBefore(LocalDate.of(1990, 1,1 ))))

}
