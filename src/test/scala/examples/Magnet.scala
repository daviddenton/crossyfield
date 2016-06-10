package examples

import io.github.daviddenton.crossyfield.{Extracted, Extraction, Extractors, Invalid, NotProvided}

import scala.language.implicitConversions

class Validation[In <: Product] private(extractors: Product, value: => In) {
  private val errors = extractors.productIterator.filter(_.isInstanceOf[Extraction[_]]).map(_.asInstanceOf[Extraction[_]]).toList.flatMap {
    case Invalid(q) => q
    case _ => Nil
  }

  def apply[Result](pf: Function[In, Result]): Extraction[Result] = if (errors.isEmpty) Extracted(pf(value)) else Invalid(errors)
}

object Validation {

  type Ex[T] = Extraction[T]
  type Op[T] = Option[T]

  private def extract[T](e: Ex[T]): Option[T] = e match {
    case Extracted(v) => Some(v)
    case NotProvided => None
    case Invalid(_) => None
  }

  def apply[In <: Product](validation: Validation[In]) = validation

  implicit def tuple2ToMagnet[A, B](in: (Ex[A], Ex[B])): Validation[(Op[A], Op[B])] =
    new Validation(in, (extract(in._1), extract(in._2)))

  implicit def tuple3ToMagnet[A, B, C](in: (Ex[A], Ex[B], Ex[C])): Validation[(Op[A], Op[B], Op[C])] =
    new Validation(in, (extract(in._1), extract(in._2), extract(in._3)))

  implicit def tuple4ToMagnet[A, B, C, D](in: (Ex[A], Ex[B], Ex[C], Ex[D])): Validation[(Op[A], Op[B], Op[C], Op[D])] =
    new Validation(in, (extract(in._1), extract(in._2), extract(in._3), extract(in._4)))

  implicit def tuple5ToMagnet[A, B, C, D, E](in: (Ex[A], Ex[B], Ex[C], Ex[D], Ex[E])): Validation[(Op[A], Op[B], Op[C], Op[D], Op[E])] =
    new Validation(in, (extract(in._1), extract(in._2), extract(in._3), extract(in._4), extract(in._5)))
}


object MagnetApp extends App {
  private val int = Extractors.string.optional.int('asd)
  private val bbb = Extractors.string.optional.int('asd2)

  val e = Validation(int <--? "", bbb <--? "") {
    case (a, b) => "as " + a + b
  }

  e match {
    case Extracted(b) => println(b)
    case NotProvided => println("nothing")
    case Invalid(i) => println(i)
  }

}
