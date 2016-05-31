package examples

import scala.language.implicitConversions

sealed trait ExtractedValue[T]

class ArgExtractor[T](desired: Manifest[T]) {
  def unapply[X](c: X)(implicit m: Manifest[X]): Option[T] = {
    def sameArgs = desired.typeArguments.zip(m.typeArguments).forall { case (d, actual) => d >:> actual }
    if (desired >:> m && sameArgs) Some(c.asInstanceOf[T])
    else None
  }
}

class Magnet[A](a: A) {
  def apply(implicit desired: Manifest[A]) = new Function[PartialFunction[A, String], String] {
    val Args = new ArgExtractor[A](desired)
    override def apply(pf: PartialFunction[A, String]): String = pf(Args.unapply(a).get)
  }
}

object Magnet {

  def apply[A](magnet: Magnet[A])(implicit desired: Manifest[A]) = magnet.apply(desired)

  implicit def t2ToMagnet[A, B](in: (A, B))(implicit desired: Manifest[A]): Magnet[(A, B)] = new Magnet[(A, B)](in)

  implicit def t3ToMagnet[A, B, C](in: (A, B, C))(implicit desired: Manifest[A]): Magnet[(A, B, C)] = new Magnet[(A, B, C)](in)
}

object MagnetApp extends App {

  private val it1 = Magnet(Option(1), Option(3))
  private val out = it1 {
    case (a, b) => "asd " + a + b
  }
  println(out)

}
