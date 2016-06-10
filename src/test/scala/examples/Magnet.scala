package examples

import io.github.daviddenton.crossyfield.{Extracted, Extraction, Extractors, Invalid}

import scala.language.implicitConversions


class ExtractionM[In <: Product](val extractors: Product, val fn: () => In) {
  type Result = String

  private def toErrors(tuple: Product) = tuple
    .productIterator
    .filter(_.isInstanceOf[Extraction[_]])
    .map(_.asInstanceOf[Extraction[_]]).
    toList.flatMap {
    case Invalid(q) => q
    case _ => Nil
  }

  def apply(a: Product) = new Function[PartialFunction[In, Result], Extraction[Result]] {
    override def apply(pf: PartialFunction[In, Result]): Extraction[Result] = if (toErrors(a).isEmpty) Extracted(pf(fn())) else Invalid(toErrors(a))
  }

}

object ExtractionM {

  def apply[In <: Product](extractionMagnet: ExtractionM[In])(implicit desired: Manifest[In]) = extractionMagnet(extractionMagnet.extractors)

  implicit def t2ToMagnet[A, B](in: (Extraction[A], Extraction[B])): ExtractionM[(Option[A], Option[B])] = {
    new ExtractionM[(Option[A], Option[B])](in, () => (in._1.get, in._2.get))
  }
}


object MagnetApp extends App {

  private val int = Extractors.string.optional.int('asd)
  private val bbb = Extractors.string.required.int('asd2)

  private val m = ExtractionM(
    int <--? "",
    bbb <--? "321")
  println(m {
    case (a, b) => "asd " + a + b
  })

}
