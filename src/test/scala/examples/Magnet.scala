package examples

import io.github.daviddenton.crossyfield.{Extracted, Extraction, Extractors, Invalid}

import scala.language.implicitConversions


class ExtractionM[In <: Product](extractors: Product, fn: () => In) extends Function[PartialFunction[In, String], Extraction[String]]{
  private val toErrors = extractors
    .productIterator
    .filter(_.isInstanceOf[Extraction[_]])
    .map(_.asInstanceOf[Extraction[_]]).
    toList.flatMap {
    case Invalid(q) => q
    case _ => Nil
  }

    override def apply(pf: PartialFunction[In, String]): Extraction[String] = if (toErrors.isEmpty) Extracted(pf(fn())) else Invalid(toErrors)
}

object ExtractionM {

  def apply[In <: Product](extractionMagnet: ExtractionM[In])(implicit desired: Manifest[In]) = extractionMagnet

  implicit def t2ToMagnet[A, B](in: (Extraction[A], Extraction[B])): ExtractionM[(Option[A], Option[B])] = {
    new ExtractionM[(Option[A], Option[B])](in, () => (in._1.get, in._2.get))
  }
}


object MagnetApp extends App {

  private val int = Extractors.string.optional.int('asd)
  private val bbb = Extractors.string.required.int('asd2)

  private val m = ExtractionM(
    int <--? "qwe",
    bbb <--? "321")
  println(m {
    case (a, b) => "asd " + a + b
  })

}
