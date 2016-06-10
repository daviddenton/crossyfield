package examples

import io.github.daviddenton.crossyfield.{Extracted, Extraction, Extractors, Invalid}

import scala.language.implicitConversions


abstract class ExtractionM[In <: Product](val a: Product) {
  def t: In
  type Result = String

  private def toErrors(tuple: Product) = tuple.productIterator.map(_.asInstanceOf[Extraction[_]]).toList.flatMap {
    case Invalid(q) => q
    case _ => Nil
  }

  def bob(a: Product, in: In) = new Function[PartialFunction[In, Result], Extraction[Result]] {
    override def apply(pf: PartialFunction[In, Result]): Extraction[Result] = if (toErrors(a).isEmpty) Extracted(pf(in)) else Invalid(toErrors(in))
  }

}

object ExtractionM {

  def apply[In <: Product](extractionMagnet: ExtractionM[In])(implicit desired: Manifest[In]) = extractionMagnet.bob(extractionMagnet.a, extractionMagnet.t)

  implicit def t2ToMagnet[A, B](in: (Extraction[A], Extraction[B])): ExtractionM[(Option[A], Option[B])] = new ExtractionM[(Option[A], Option[B])](in) {
    def t = (in._1.get, in._2.get)
  }
}


object MagnetApp extends App {

  private val int = Extractors.string.required.int('asd)
  private val bbb = Extractors.string.required.int('asd2)

  private val m = ExtractionM((
    int <--? "1223",
    bbb <--? "321"))
  println(m {
    case (a, b) => "asd " + a + b
  })

}
