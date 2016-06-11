package io.github.daviddenton.crossyfield

object Extraction {

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
      case ExtractionFailed(ip) => ExtractionFailed(ip)
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
case class ExtractionFailed(error: Error) extends Extraction[Nothing] {
  def flatMap[O](f: Option[Nothing] => Extraction[O]) = ExtractionFailed(error)

  override def map[O](f: Option[Nothing] => O) = ExtractionFailed(error)

  override def orDefault[T](f: => T): Extraction[T] = this
}

