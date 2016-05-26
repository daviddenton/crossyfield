package io.github.daviddenton.crossyfield

trait Validatable[-From, +T] {
  def <--?(from: From): Validation[T]

  final def <--?(from: From, reason: String, predicate: T => Boolean): Validation[T] =
    <--?(from).flatMap[T](v => if (v.map(predicate).getOrElse(true)) Validation(v) else ValidationFailed(reason))

  final def extract(from: From): Validation[T] = <--?(from)

  final def extract(from: From, reason: String, predicate: T => Boolean): Validation[T] = <--?(from, reason, predicate)
}

object Validatable {
  def mk[From, T](fn: From => Validation[T]): Validatable[From, T] = new Validatable[From, T] {
    override def <--?(from: From): Validation[T] = fn(from)
  }
}

/**
  * Result of an attempt to validate an object from a target
  */
sealed trait Validation[+T] {
  def flatMap[O](f: Option[T] => Validation[O]): Validation[O]

  def map[O](f: Option[T] => O): Validation[O]

  def orDefault[O >: T](f: => O): Validation[O]
}

object Validation {
  /**
    * Utility method for combining the results of many Validation into a single Validation, simply to get an overall
    * validation result in the case of failure.
    */
  def combine(extractions: Seq[Validation[_]]): Validation[Nothing] = {
    val missingOrFailed = extractions.flatMap {
      case ValidationFailed(ip) => ip
      case _ => Nil
    }
    if (missingOrFailed.isEmpty) Ignored else ValidationFailed(missingOrFailed)
  }

  /**
    * Wraps in a successful Validation - this assumes the item was not mandatory.
    */
  def apply[T](t: Option[T]): Validation[T] = t.map(Validated(_)).getOrElse(Ignored)

  /**
    * For optional cases, you can use this to convert an Validation(None) -> Ignored
    */
  def flatten[T](extraction: Validation[Option[T]]): Validation[T] =
    extraction match {
      case Validated(opt) => opt.map(Validated(_)).getOrElse(Ignored)
      case Ignored => Ignored
      case ValidationFailed(ip) => ValidationFailed(ip)
    }
}

/**
  * Represents a object which was provided and validated successfully
  */
case class Validated[T](value: T) extends Validation[T] {
  def flatMap[O](f: Option[T] => Validation[O]) = f(Some(value))

  override def map[O](f: Option[T] => O) = Validated(f(Some(value)))

  override def orDefault[O >: T](f: => O): Validation[O] = this
}

/**
  * Represents an Ignored object which was not provided - this is a non-failing case
  */
object Ignored extends Validation[Nothing] {

  override def toString = "Optional"

  def flatMap[O](f: Option[Nothing] => Validation[O]) = f(None)

  override def map[O](f: Option[Nothing] => O) = Validated(f(None))

  override def orDefault[T](f: => T): Validation[T] = Validated(f)
}

/**
  * Represents a object which could not be extracted due to it being invalid or missing when required
  */
case class ValidationFailed(invalid: Seq[String]) extends Validation[Nothing] {
  def flatMap[O](f: Option[Nothing] => Validation[O]) = ValidationFailed(invalid)

  override def map[O](f: Option[Nothing] => O) = ValidationFailed(invalid)

  override def orDefault[T](f: => T): Validation[T] = this
}

object ValidationFailed {
  def apply(p: String): ValidationFailed = ValidationFailed(Seq(p))
}
