package io.github.daviddenton.crossyfield

import scala.util.{Failure, Success, Try}

case class CollectExtractionError()

trait CollectExtractor[-From, +T] {
  val identifier: Symbol

  /**
    * Performs extraction
    */
  def <--?(from: From): CollectExtraction[T]

  /**
    * Performs extraction. Synonym for <--?().
    */
  final def extract(from: From): CollectExtraction[T] = <--?(from)

  /**
    * Performs extraction and applies the predicate to achieve a result.
    */
  final def <--?(from: From, error: String, predicate: T => Boolean): CollectExtraction[T] =
    <--?(from).flatMap[T](v => if (v.map(predicate).getOrElse(true)) CollectExtraction(v) else CollectInvalid(identifier -> error))

  /**
    * Performs extraction and applies the predicate to achieve a result. Synonym for <--?().
    */
  final def extract(from: From, reason: String, predicate: T => Boolean): CollectExtraction[T] = <--?(from, reason, predicate)
}

object CollectExtractor {

  /**
    * Constructs a simple Mandatory Extractor from applying the passed Extractor function
    */
  def mk[From, T](id: Symbol)(fn: From => CollectExtraction[T]): Extractor[From, T] = new Extractor[From, T] {
    override val identifier = id

    override def <--?(from: From): CollectExtraction[T] = fn(from)
  }

  /**
    * Constructs a simple Mandatory Extractor for from a function, returns either CollectExtracted or CollectInvalid upon
    * an failure from the function
    */
  def mk[From, T](id: Symbol, message: String, fn: From => T): Extractor[From, T] = new Extractor[From, T] {
    override val identifier: Symbol = id

    override def <--?(from: From): CollectExtraction[T] = Try(fn(from)) match {
      case Success(value) => CollectExtracted(value)
      case Failure(e) => CollectInvalid(identifier -> message)
    }
  }
}

/**
  * Result of an attempt to extract an object from a target
  */
sealed trait CollectExtraction[+T] {
  def flatMap[O](f: Option[T] => CollectExtraction[O]): CollectExtraction[O]

  def map[O](f: Option[T] => O): CollectExtraction[O]

  def orDefault[O >: T](f: => O): CollectExtraction[O]
}

object CollectExtraction {

  /**
    * Collect errors together from several extractions.
    */
  def collectErrors(extractions: CollectExtraction[_]*): Seq[(Symbol, String)] = <--?(extractions) match {
    case CollectInvalid(ip) => ip
    case _ => Nil
  }

  /**
    * Utility method for combining the results of many CollectExtraction into a single CollectExtraction, simply to get an overall
    * extraction result in the case of failure.
    */
  def <--?(extractions: Seq[CollectExtraction[_]]): CollectExtraction[Nothing] = {
    val missingOrFailed = extractions.flatMap {
      case CollectInvalid(ip) => ip
      case _ => Nil
    }
    if (missingOrFailed.isEmpty) CollectNotProvided else CollectInvalid(missingOrFailed)
  }

  /**
    * Wraps in a successful CollectExtraction - this assumes the object was not mandatory.
    */
  def apply[T](t: Option[T]): CollectExtraction[T] = t.map(CollectExtracted(_)).getOrElse(CollectNotProvided)

  /**
    * For optional cases, you can use this to convert an CollectExtraction(None) -> CollectNotProvided
    */
  def flatten[T](extraction: CollectExtraction[Option[T]]): CollectExtraction[T] =
    extraction match {
      case CollectExtracted(opt) => opt.map(CollectExtracted(_)).getOrElse(CollectNotProvided)
      case CollectNotProvided => CollectNotProvided
      case CollectInvalid(ip) => CollectInvalid(ip)
    }
}

/**
  * Represents a object which was provided and extracted successfully.
  */
case class CollectExtracted[T](value: T) extends CollectExtraction[T] {
  def flatMap[O](f: Option[T] => CollectExtraction[O]) = f(Some(value))

  override def map[O](f: Option[T] => O) = CollectExtracted(f(Some(value)))

  override def orDefault[O >: T](f: => O): CollectExtraction[O] = this
}

/**
  * Represents an object which was optional and missing. Ie. still a passing case.
  */
object CollectNotProvided extends CollectExtraction[Nothing] {

  override def toString = "CollectNotProvided"

  def flatMap[O](f: Option[Nothing] => CollectExtraction[O]) = f(None)

  override def map[O](f: Option[Nothing] => O) = CollectExtracted(f(None))

  override def orDefault[T](f: => T): CollectExtraction[T] = CollectExtracted(f)
}

/**
  * Represents a object which could not be extracted due to it being invalid or missing when required.
  */
case class CollectInvalid(invalid: Seq[(Symbol, String)]) extends CollectExtraction[Nothing] {
  def flatMap[O](f: Option[Nothing] => CollectExtraction[O]) = CollectInvalid(invalid)

  override def map[O](f: Option[Nothing] => O) = CollectInvalid(invalid)

  override def orDefault[T](f: => T): CollectExtraction[T] = this
}

object CollectInvalid {
  def apply(p: (Symbol, String)): CollectInvalid = CollectInvalid(Seq(p))
}
