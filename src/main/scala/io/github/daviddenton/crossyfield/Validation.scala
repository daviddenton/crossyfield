package io.github.daviddenton.crossyfield

sealed trait Validation[+T]

case class Validated[T](value: T) extends Validation[T]

/**
  * Represents a object which could not be extracted due to it being invalid or missing when required.
  */
case class ValidationFailed(errors: Seq[Error]) extends Validation[Nothing]

object ValidationFailed {
  def apply(p: Error): ValidationFailed = ValidationFailed(Seq(p))
}

