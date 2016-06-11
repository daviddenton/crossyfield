package examples

import java.time.LocalDate

import io.github.daviddenton.crossyfield.{Extractors, Validator}

import scala.util.Try

object Examples extends App {

  /**
    * Simple case class which represents an item we wish to extract and validate.
    * The business rule is that IF the endDate is specified, it must be AFTER the start
    */
  case class Range(startDate: LocalDate, endDate: Option[LocalDate])

  /**
    * Using a standard for comprehension - we don't get nice error messages on failure, and the logic of the comprehension
    * gets hairier and hairier as we add rules. Because of the nature of the comprehension, the first error will short-circuit
    * and validation stops.
    */
  def originalStyle(in: String) = {
    val dates = in.split(",")

    def mandatory[T](in: => T) = Try(in)

    def optional[T](in: => T) = Try(in).map(Option(_)).recover[Option[T]] { case _ => None }

    def checkDates(start: LocalDate, end: Option[LocalDate]) = end.isEmpty || end.exists(_.isAfter(start))

    val result = for {
      start <- mandatory(LocalDate.parse(dates(0)))
      end <- optional(LocalDate.parse(in)) if checkDates(start, end)
    } yield Range(start, end)
    result.toString
  }

  /**
    * Using CrossyField Extractor: failure modes are combined for invalid and rule failures. Extractors are also
    * embeddable, see further down for an example. Validation is also short-circuited as per the above example.
    */
  def simpleCrossFieldValidation(in: String) = {
    val dates = in.split(",")

    val result = for {
      start <- Extractors.string.required.localDate('startDate) <--? dates(0)
      end <- Extractors.string.optional.localDate('endDate) <--?(dates(1), "end date <= start", _.isAfter(start.get))
    } yield Range(start.get, end)
    result.toString
  }

  /**
    * Using CrossyField Validator: collects errors into a list, or returns the successful case.
    */
  def collectingErrors(in: String) = {
    val dates = in.split(",")

    Validator.mk(
      Extractors.string.required.localDate('startDate) <--? dates(0),
      Extractors.string.optional.localDate('endDate) <--? dates(1)
    ) {
      case (start, end) => Range(start.get, end)
    }.toString
  }

  def runWith(title: String, fn: String => String): Unit = {
    println(title)
    (1 to title.length) foreach { _ => print("-") }
    println("\nall ok: " + fn("2000-01-02,2001-01-02,someothervalues"))
    println("missing end date: " + fn("2000-01-02,,someothervalues"))
    println("(CROSS FIELD) end date is before start: " + fn("2000-01-02,1999-01-02,someothervalues"))
    println("(ERROR COLLECTION) invalid dates: " + fn("qqqq-01-02,rrrr-01-02,someothervalues"))
    println("\n")
  }

  runWith("original style", originalStyle)
  runWith("cross field validation", simpleCrossFieldValidation)
  runWith("error collection ", collectingErrors)

}
