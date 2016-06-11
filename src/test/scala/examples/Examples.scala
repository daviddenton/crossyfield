package examples

import java.time.LocalDate

import scala.util.Try

object Examples extends App {

  /**
    * Simple case class which represents an item we wish to extract and validate
    */
  case class Range(startDate: LocalDate, endDate: Option[LocalDate])

  def originalStyle(in: String) = {
    val dates = in.split(",")

    val range = for {
      start <- Try(LocalDate.parse(dates(0)))
      end <- Try(LocalDate.parse(dates(1))).map(Option(_)).recover[Option[LocalDate]] { case _ => None }
      if end.isEmpty || end.exists(_.isAfter(start))
    } yield Range(start, end)
    range.toString
  }

  def runWith(title: String, fn: String => String): Unit = {
    println(title)
    (1 to title.length) foreach {_ => print("-") }
    println("\nall ok: " + fn("2000-01-02,2001-01-02"))
    println("missing end date: " + fn("2000-01-02,"))
    println("end date is before start: " + fn("2000-01-02,1999-01-02"))
    println("invalid start date: " + fn("qqqq-01-02,2000-01-02,2000-01-02"))
  }

  runWith("original style" , originalStyle)

}
