package examples

import scala.language.implicitConversions

sealed trait ExtractedValue[T]

class Args2[C](implicit desired: Manifest[C]) {
  def unapply[X](c: X)(implicit m: Manifest[X]): Option[C] = {
    def sameArgs = desired.typeArguments.zip(m.typeArguments).forall { case (d, actual) => d >:> actual }
    if (desired >:> m && sameArgs) Some(c.asInstanceOf[C])
    else None
  }
}

case class Extracted[T](t: T) extends ExtractedValue[T]

case class Missing[T]() extends ExtractedValue[T]

class Magnet[A] {
  def apply(): Function[PartialFunction[A, String], Function[A, String]] = pf => in => pf(in)
}

object Magnet {

  implicit def t2ToMagnet[A, B](a: (A, B)): Magnet[(A, B)] = new Magnet[(A, B)]

  implicit def t3ToMagnet[A, B, C](a: (A, B, C)): Magnet[(A, B, C)] = new Magnet[(A, B, C)]
}

object MagnetApp extends App {

  def doIt[A](magnet: Magnet[A]) = magnet.apply()

  val a = new Args2[(Int, Int)]()
  private val it1 = doIt(1, 3)
  private val it = it1 {
    case a(a, b) => "asd " + a + b
  }

  println(it(1, 2))

}
