package io.github.daviddenton.crossyfield

import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.util.UUID

import io.github.daviddenton.crossyfield.Validator.ValidationError

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait Validator[-From, +T] {
  val identifier: Symbol

  /**
    * Performs validation
    */
  def <--?(from: From): Validation[T]

  /**
    * Performs validation. Synonym for <--?().
    */
  final def validate(from: From): Validation[T] = <--?(from)

  /**
    * Performs validation and applies the predicate to achieve a result.
    */
  final def <--?(from: From, error: String, predicate: T => Boolean): Validation[T] =
    <--?(from).flatMap[T](v => if (v.map(predicate).getOrElse(true)) Validation(v) else Invalid((identifier, error)))

  /**
    * Performs validation and applies the predicate to achieve a result. Synonym for <--?().
    */
  final def validate(from: From, reason: String, predicate: T => Boolean): Validation[T] = <--?(from, reason, predicate)
}

object Validator {

  type ValidationError = (Symbol, String)

  /**
    * Constructs a simple Mandatory validator from applying the passed Validator function
    */
  def mk[From, T](id: Symbol)(fn: From => Validation[T]): Validator[From, T] = new Validator[From, T] {
    override val identifier = id

    override def <--?(from: From): Validation[T] = fn(from)
  }

  /**
    * Constructs a simple Mandatory validator for from a function, returns either Validated or Invalid upon
    * an failure from the function
    */
  def mk[From, T](id: Symbol, message: String, fn: From => T): Validator[From, T] = new Validator[From, T] {
    override val identifier: Symbol = id

    override def <--?(from: From): Validation[T] = Try(fn(from)) match {
      case Success(value) => Validated(value)
      case Failure(e) => Invalid(identifier, message)
    }
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
    * Collect errors together from several validations.
    */
  def collectErrors(validations: Validation[_]*): Seq[ValidationError] = <--?(validations) match {
    case Invalid(ip) => ip
    case _ => Nil
  }

  /**
    * Utility method for combining the results of many Validation into a single Validation, simply to get an overall
    * validation result in the case of failure.
    */
  def <--?(validations: Seq[Validation[_]]): Validation[Nothing] = {
    val missingOrFailed = validations.flatMap {
      case Invalid(ip) => ip
      case _ => Nil
    }
    if (missingOrFailed.isEmpty) Ignored else Invalid(missingOrFailed)
  }

  /**
    * Wraps in a successful Validation - this assumes the object was not mandatory.
    */
  def apply[T](t: Option[T]): Validation[T] = t.map(Validated(_)).getOrElse(Ignored)

  /**
    * For optional cases, you can use this to convert an Validation(None) -> Ignored
    */
  def flatten[T](extraction: Validation[Option[T]]): Validation[T] =
    extraction match {
      case Validated(opt) => opt.map(Validated(_)).getOrElse(Ignored)
      case Ignored => Ignored
      case Invalid(ip) => Invalid(ip)
    }
}

/**
  * Represents a object which was provided and validated successfully.
  */
case class Validated[T](value: T) extends Validation[T] {
  def flatMap[O](f: Option[T] => Validation[O]) = f(Some(value))

  override def map[O](f: Option[T] => O) = Validated(f(Some(value)))

  override def orDefault[O >: T](f: => O): Validation[O] = this
}

/**
  * Represents an object which was optional and missing. Ie. still a passing case.
  */
object Ignored extends Validation[Nothing] {

  override def toString = "Ignored"

  def flatMap[O](f: Option[Nothing] => Validation[O]) = f(None)

  override def map[O](f: Option[Nothing] => O) = Validated(f(None))

  override def orDefault[T](f: => T): Validation[T] = Validated(f)
}

/**
  * Represents a object which could not be validated due to it being invalid or missing when mandatory.
  */
case class Invalid(invalid: Seq[ValidationError]) extends Validation[Nothing] {
  def flatMap[O](f: Option[Nothing] => Validation[O]) = Invalid(invalid)

  override def map[O](f: Option[Nothing] => O) = Invalid(invalid)

  override def orDefault[T](f: => T): Validation[T] = this
}

object Invalid {
  def apply(p: ValidationError): Invalid = Invalid(Seq(p))
}

/**
  * Convenience Validators
  */
object Validators {

  object string {
    def int(id: Symbol, msg: String = "invalid int") = Validator.mk(id, msg, (s: String) => s.toInt)

    def double(id: Symbol, msg: String = "invalid double") = Validator.mk(id, msg, (s: String) => s.toDouble)

    def long(id: Symbol, msg: String = "invalid long") = Validator.mk(id, msg, (s: String) => s.toLong)

    def float(id: Symbol, msg: String = "invalid float") = Validator.mk(id, msg, (s: String) => s.toFloat)

    def uuid(id: Symbol, msg: String = "invalid uuid") = Validator.mk(id, msg, UUID.fromString)

    def bigDecimal(id: Symbol, msg: String = "invalid bigDecimal") = Validator.mk(id, msg, (s: String) => BigDecimal(s))

    def boolean(id: Symbol, msg: String = "invalid boolean") = Validator.mk(id, "invalid float", (s: String) => s.toBoolean)

    def char(id: Symbol, msg: String = "invalid boolean") = Validator.mk(id, msg, (s: String) => s.charAt(0))

    def localDateTime(id: Symbol, msg: String = "invalid localDateTime") = Validator.mk(id, msg, LocalDateTime.parse)

    def localDate(id: Symbol, msg: String = "invalid localDate") = Validator.mk(id, msg, LocalDate.parse)

    def zonedDateTime(id: Symbol, msg: String = "invalid zonedDateTime") = Validator.mk(id, msg, ZonedDateTime.parse)
  }

}