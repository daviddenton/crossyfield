package io.github.daviddenton.crossyfield

import scala.language.implicitConversions

class Validator[In <: Product] private(extractors: Product, value: => In) {
  private val errors = extractors.productIterator.filter(_.isInstanceOf[Extraction[_]]).map(_.asInstanceOf[Extraction[_]]).toList.flatMap {
    case ExtractionFailed(q) => q
    case _ => Nil
  }

  def apply[Result](pf: Function[In, Result]): Validation[Result] =
    if (errors.isEmpty) Validated(pf(value)) else ValidationFailed(errors)
}

object Validator {

  private def extract[T](e: Extraction[T]): Option[T] = e match {
    case Extracted(v) => Some(v)
    case NotProvided => None
    case ExtractionFailed(_) => None
  }

  def mk[In <: Product](validation: Validator[In]) = validation

  implicit def tuple2ToValidation[A, B](in: (Extraction[A], Extraction[B])): Validator[(Option[A], Option[B])] =
    new Validator(in, (extract(in._1), extract(in._2)))

  implicit def tuple3ToValidation[A, B, C](in: (Extraction[A], Extraction[B], Extraction[C])): Validator[(Option[A], Option[B], Option[C])] =
    new Validator(in, (extract(in._1), extract(in._2), extract(in._3)))

  implicit def tuple4ToValidation[A, B, C, D](in: (Extraction[A], Extraction[B], Extraction[C], Extraction[D])): Validator[(Option[A], Option[B], Option[C], Option[D])] =
    new Validator(in, (extract(in._1), extract(in._2), extract(in._3), extract(in._4)))

  implicit def tuple5ToValidation[A, B, C, D, E](in: (Extraction[A], Extraction[B], Extraction[C], Extraction[D], Extraction[E])): Validator[(Option[A], Option[B], Option[C], Option[D], Option[E])] =
    new Validator(in, (extract(in._1), extract(in._2), extract(in._3), extract(in._4), extract(in._5)))

  implicit def tuple6ToValidation[A, B, C, D, E, F](in: (Extraction[A], Extraction[B], Extraction[C], Extraction[D], Extraction[E], Extraction[F])): Validator[(Option[A], Option[B], Option[C], Option[D], Option[E], Option[F])] =
    new Validator(in, (extract(in._1), extract(in._2), extract(in._3), extract(in._4), extract(in._5), extract(in._6)))
}

