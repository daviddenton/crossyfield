package io.github.daviddenton.crossyfield

import scala.util.{Failure, Success, Try}


trait Extractor2[-From, +T] {
  val identifier: Symbol

  /**
    * Performs Extraction2
    */
  def <--?(from: From): Extraction2[T]

  /**
    * Performs Extraction2. Synonym for <--?().
    */
  final def extract(from: From): Extraction2[T] = <--?(from)

  /**
    * Performs Extraction2 and applies the predicate to achieve a result.
    */
  final def <--?(from: From, error: String, predicate: T => Boolean): Extraction2[T] =
    <--?(from).flatMap[T](v => if (predicate(v)) Extracted2(v) else Extraction2Failed((identifier, error)))

  /**
    * Performs Extraction2 and applies the predicate to achieve a result. Synonym for <--?().
    */
  final def extract(from: From, reason: String, predicate: T => Boolean): Extraction2[T] = <--?(from, reason, predicate)
}

object Extractor2 {

  /**
    * Constructs a simple Mandatory Extractor2 from applying the passed Extractor2 function
    */
  def mk[From, T](id: Symbol)(fn: From => Extraction2[T]): Extractor2[From, T] = new Extractor2[From, T] {
    override val identifier = id

    override def <--?(from: From): Extraction2[T] = fn(from)
  }

  /**
    * Constructs a simple Mandatory Extractor2 for from a function, returns either Extracted or Invalid upon
    * an failure from the function
    */
  def mk[From, T](id: Symbol, message: String, fn: From => T): Extractor2[From, T] = new Extractor2[From, T] {
    override val identifier: Symbol = id

    override def <--?(from: From): Extraction2[T] = Try(fn(from)) match {
      case Success(value) => Extracted2(value)
      case Failure(e) => Extraction2Failed(identifier, message)
    }
  }
}

object Extraction2 {

}


/**
  * Result of an attempt to extract an object from a target
  */
sealed trait Extraction2[+T] {
  def flatMap[O](f: T => Extraction2[O]): Extraction2[O]

  def map[O](f: T => O): Extraction2[O]

  def orDefault[O >: T](f: => O): Extraction2[O]
}

/**
  * Represents a object which was provided and extracted successfully.
  */
case class Extracted2[T](value: T) extends Extraction2[T] {
  def flatMap[O](f: T => Extraction2[O]) = f(value)

  override def map[O](f: T => O) = Extracted2(f(value))

  override def orDefault[O >: T](f: => O): Extraction2[O] = this
}

/**
  * Represents a object which could not be extracted due to it being invalid or missing when required.
  */
case class Extraction2Failed(error: Error) extends Extraction2[Nothing] {
  def flatMap[O](f: Nothing => Extraction2[O]) = Extraction2Failed(error)

  override def map[O](f: Nothing => O) = Extraction2Failed(error)

  override def orDefault[T](f: => T): Extraction2[T] = this
}

