package io.github.daviddenton.crossyfield

import io.github.daviddenton.crossyfield.Extraction.ExtractionError

object Extraction {

  type ExtractionError = (Symbol, String)

  /**
    * Utility method for combining the results of many Extraction into a single Extraction, simply to get an overall
    * extraction result in the case of failure.
    */
  def <--?(extractions: Seq[Extraction[_]]): Extraction[Nothing] = {
    val missingOrFailed = extractions.flatMap {
      case Errors(ip) => ip
      case _ => Nil
    }
    if (missingOrFailed.isEmpty) NotProvided else Errors(missingOrFailed)
  }

  /**
    * Wraps in a successful Extraction - this assumes the object was not mandatory.
    */
  def apply[T](t: Option[T]): Extraction[T] = t.map(Successful(_)).getOrElse(NotProvided)

  /**
    * For optional cases, you can use this to convert an Extraction(None) -> NotProvided
    */
  def flatten[T](extraction: Extraction[Option[T]]): Extraction[T] =
    extraction match {
      case Successful(opt) => opt.map(Successful(_)).getOrElse(NotProvided)
      case NotProvided => NotProvided
      case Errors(ip) => Errors(ip)
    }
}


/**
  * Result of an attempt to extract an object from a target
  */
sealed trait Extraction[+T] {
  def flatMap[O](f: Option[T] => Extraction[O]): Extraction[O]

  def map[O](f: Option[T] => O): Extraction[O]

  def orDefault[O >: T](f: => O): Extraction[O]
}

/**
  * Represents a object which was provided and extracted successfully.
  */
case class Successful[T](value: T) extends Extraction[T] {
  def flatMap[O](f: Option[T] => Extraction[O]) = f(Some(value))

  override def map[O](f: Option[T] => O) = Successful(f(Some(value)))

  override def orDefault[O >: T](f: => O): Extraction[O] = this
}

/**
  * Represents an object which was optional and missing. Ie. still a passing case.
  */
object NotProvided extends Extraction[Nothing] {

  override def toString = "NotProvided"

  def flatMap[O](f: Option[Nothing] => Extraction[O]) = f(None)

  override def map[O](f: Option[Nothing] => O) = Successful(f(None))

  override def orDefault[T](f: => T): Extraction[T] = Successful(f)
}

/**
  * Represents a object which could not be extracted due to it being invalid or missing when required.
  */
case class Errors(invalid: Seq[ExtractionError]) extends Extraction[Nothing] {
  def flatMap[O](f: Option[Nothing] => Extraction[O]) = Errors(invalid)

  override def map[O](f: Option[Nothing] => O) = Errors(invalid)

  override def orDefault[T](f: => T): Extraction[T] = this
}

object Errors {
  def apply(p: ExtractionError): Errors = Errors(Seq(p))
}

