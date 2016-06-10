package io.github.daviddenton.crossyfield

import scala.language.implicitConversions

class Validation[In <: Product] private(extractors: Product, value: => In) {
  private val errors = extractors.productIterator.filter(_.isInstanceOf[Extraction[_]]).map(_.asInstanceOf[Extraction[_]]).toList.flatMap {
    case Invalid(q) => q
    case _ => Nil
  }

  def apply[Result](pf: Function[In, Result]): ExtractionResult[Result] = if (errors.isEmpty) Successful(pf(value)) else Errors(errors)
}

object Validation {

  private def extract[T](e: Extraction[T]): Option[T] = e match {
    case Extracted(v) => Some(v)
    case NotProvided => None
    case Invalid(_) => None
  }

  def mk[In <: Product](validation: Validation[In]) = validation

  implicit def tuple2ToValidation[A, B](in: (Extraction[A], Extraction[B])): Validation[(Option[A], Option[B])] =
    new Validation(in, (extract(in._1), extract(in._2)))

  implicit def tuple3ToValidation[A, B, C](in: (Extraction[A], Extraction[B], Extraction[C])): Validation[(Option[A], Option[B], Option[C])] =
    new Validation(in, (extract(in._1), extract(in._2), extract(in._3)))

  implicit def tuple4ToValidation[A, B, C, D](in: (Extraction[A], Extraction[B], Extraction[C], Extraction[D])): Validation[(Option[A], Option[B], Option[C], Option[D])] =
    new Validation(in, (extract(in._1), extract(in._2), extract(in._3), extract(in._4)))

  implicit def tuple5ToValidation[A, B, C, D, E](in: (Extraction[A], Extraction[B], Extraction[C], Extraction[D], Extraction[E])): Validation[(Option[A], Option[B], Option[C], Option[D], Option[E])] =
    new Validation(in, (extract(in._1), extract(in._2), extract(in._3), extract(in._4), extract(in._5)))
}

