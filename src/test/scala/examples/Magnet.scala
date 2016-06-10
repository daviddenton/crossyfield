package examples

import io.github.daviddenton.crossyfield.{Extracted, Extraction, Extractors, Invalid, NotProvided}

import scala.language.implicitConversions

class Validation[In <: Product](extractors: Product, fn: () => In) {
  private val errors = extractors.productIterator.filter(_.isInstanceOf[Extraction[_]]).map(_.asInstanceOf[Extraction[_]]).toList.flatMap {
    case Invalid(q) => q
    case _ => Nil
  }

  def apply[Result](pf: PartialFunction[In, Result]): Extraction[Result] = if (errors.isEmpty) Extracted(pf(fn())) else Invalid(errors)
}

object Validation {

  type E[T] = Extraction[T]

  private def getFrom[T](e: E[T]): Option[T] = e match {
    case Extracted(v) => Some(v)
    case NotProvided => None
    case Invalid(_) => None
  }

  def apply[In <: Product](validation: Validation[In]) = validation

  implicit def t2ToMagnet[A, B](in: (E[A], E[B])): Validation[(Option[A], Option[B])] = new Validation(in, () => (getFrom(in._1), getFrom(in._2)))

  implicit def t3ToMagnet[A, B, C](in: (E[A], E[B], E[C])): Validation[(Option[A], Option[B], Option[C])] = new Validation(in, () => (getFrom(in._1), getFrom(in._2), getFrom(in._3)))
}


object MagnetApp extends App {

  private val int = Extractors.string.optional.int('asd)
  private val bbb = Extractors.string.required.int('asd2)

  Validation(int <--? "123", bbb <--? "321") {
    case (a, b) => a.get + b.get
  } match {
    case Extracted(b) => println(b)
    case NotProvided => println("nothing")
    case Invalid(i) => println(i)
  }

}
