package io.github.daviddenton.crossyfield

import io.github.daviddenton.crossyfield.Extractor.ExtractionError

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait Extractor[-From, +T] {
  val identifier: Symbol

  /**
    * Performs extraction
    */
  def <--?(from: From): Extraction[T]

  /**
    * Performs extraction. Synonym for <--?().
    */
  final def extract(from: From): Extraction[T] = <--?(from)

  /**
    * Performs extraction and applies the predicate to achieve a result.
    */
  final def <--?(from: From, error: String, predicate: T => Boolean): Extraction[T] =
    <--?(from).flatMap[T](v => if (v.map(predicate).getOrElse(true)) Extraction(v) else Invalid((identifier, error)))

  /**
    * Performs extraction and applies the predicate to achieve a result. Synonym for <--?().
    */
  final def extract(from: From, reason: String, predicate: T => Boolean): Extraction[T] = <--?(from, reason, predicate)
}

object Extractor {

  type ExtractionError = (Symbol, String)

  /**
    * Constructs a simple Mandatory Extractor from applying the passed Extractor function
    */
  def mk[From, T](id: Symbol)(fn: From => Extraction[T]): Extractor[From, T] = new Extractor[From, T] {
    override val identifier = id

    override def <--?(from: From): Extraction[T] = fn(from)
  }

  /**
    * Constructs a simple Mandatory Extractor for from a function, returns either Extracted or Invalid upon
    * an failure from the function
    */
  def mk[From, T](id: Symbol, message: String, fn: From => T): Extractor[From, T] = new Extractor[From, T] {
    override val identifier: Symbol = id

    override def <--?(from: From): Extraction[T] = Try(fn(from)) match {
      case Success(value) => Extracted(value)
      case Failure(e) => Invalid(identifier, message)
    }
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

object Extraction {

  /**
    * Collect errors together from several extractions.
    */
  def collectErrors(extractions: Extraction[_]*): Seq[ExtractionError] = <--?(extractions) match {
    case Invalid(ip) => ip
    case _ => Nil
  }

  /**
    * Utility method for combining the results of many Extraction into a single Extraction, simply to get an overall
    * extraction result in the case of failure.
    */
  def <--?(extractions: Seq[Extraction[_]]): Extraction[Nothing] = {
    val missingOrFailed = extractions.flatMap {
      case Invalid(ip) => ip
      case _ => Nil
    }
    if (missingOrFailed.isEmpty) NotProvided else Invalid(missingOrFailed)
  }

  /**
    * Wraps in a successful Extraction - this assumes the object was not mandatory.
    */
  def apply[T](t: Option[T]): Extraction[T] = t.map(Extracted(_)).getOrElse(NotProvided)

  /**
    * For optional cases, you can use this to convert an Extraction(None) -> NotProvided
    */
  def flatten[T](extraction: Extraction[Option[T]]): Extraction[T] =
    extraction match {
      case Extracted(opt) => opt.map(Extracted(_)).getOrElse(NotProvided)
      case NotProvided => NotProvided
      case Invalid(ip) => Invalid(ip)
    }
}

/**
  * Represents a object which was provided and extracted successfully.
  */
case class Extracted[T](value: T) extends Extraction[T] {
  def flatMap[O](f: Option[T] => Extraction[O]) = f(Some(value))

  override def map[O](f: Option[T] => O) = Extracted(f(Some(value)))

  override def orDefault[O >: T](f: => O): Extraction[O] = this
}

/**
  * Represents an object which was optional and missing. Ie. still a passing case.
  */
object NotProvided extends Extraction[Nothing] {

  override def toString = "NotProvided"

  def flatMap[O](f: Option[Nothing] => Extraction[O]) = f(None)

  override def map[O](f: Option[Nothing] => O) = Extracted(f(None))

  override def orDefault[T](f: => T): Extraction[T] = Extracted(f)
}

/**
  * Represents a object which could not be extracted due to it being invalid or missing when required.
  */
case class Invalid(invalid: Seq[ExtractionError]) extends Extraction[Nothing] {
  def flatMap[O](f: Option[Nothing] => Extraction[O]) = Invalid(invalid)

  override def map[O](f: Option[Nothing] => O) = Invalid(invalid)

  override def orDefault[T](f: => T): Extraction[T] = this
}

object Invalid {
  def apply(p: ExtractionError): Invalid = Invalid(Seq(p))
}

