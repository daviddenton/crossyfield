package io.github.daviddenton.crossyfield

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
    <--?(from).flatMap[T](v => if (v.map(predicate).getOrElse(true)) Extraction(v) else ExtractionFailed((identifier, error)))

  /**
    * Performs extraction and applies the predicate to achieve a result. Synonym for <--?().
    */
  final def extract(from: From, reason: String, predicate: T => Boolean): Extraction[T] = <--?(from, reason, predicate)
}

object Extractor {

  /**
    * Constructs a simple Mandatory Extractor from applying the passed Extractor function
    */
  def mk[From, T](id: Symbol)(fn: From => Extraction[T]): Extractor[From, T] = new Extractor[From, T] {
    override val identifier = id

    override def <--?(from: From): Extraction[T] = fn(from)
  }

  /**
    * Constructs a simple Mandatory Extractor for from a function, returns either Extracted or ExtractionFailed upon
    * an failure from the function
    */
  def mk[From, T](id: Symbol, message: String, fn: From => T): Extractor[From, T] = new Extractor[From, T] {
    override val identifier: Symbol = id

    override def <--?(from: From): Extraction[T] = Try(fn(from)) match {
      case Success(value) => Extracted(value)
      case Failure(e) => ExtractionFailed(identifier, message)
    }
  }
}
