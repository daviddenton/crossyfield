package examples

import scala.language.implicitConversions

sealed trait ExtractedValue[T]

class Magnet[In](in: In) {
  type Result = String
  def apply(implicit desired: Manifest[In]) = new Function[PartialFunction[In, Result], Result] {
    override def apply(pf: PartialFunction[In, Result]): Result = pf(in)
  }
}

object Magnet {

  def apply[In](magnet: Magnet[In])(implicit desired: Manifest[In]) = magnet.apply(desired)

  implicit def t2ToMagnet[A, B](in: (A, B)): Magnet[(A, B)] = new Magnet[(A, B)](in)

  implicit def t3ToMagnet[A, B, C](in: (A, B, C)): Magnet[(A, B, C)] = new Magnet[(A, B, C)](in)
}

object MagnetApp extends App {
  private val it1 = Magnet(Option(1), Option(3))
  private val out = it1 {
    case (a, b) => "asd " + a + b
  }

  println(out)

}
