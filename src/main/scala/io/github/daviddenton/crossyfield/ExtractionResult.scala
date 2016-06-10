package io.github.daviddenton.crossyfield

import io.github.daviddenton.crossyfield.ExtractionResult.ExtractionError

object ExtractionResult {
  type ExtractionError = (Symbol, String)
}

sealed trait ExtractionResult[+T]

case class Successful[T](result: T) extends ExtractionResult[T]

case class Errors(errors: Seq[ExtractionError]) extends ExtractionResult[Nothing]
