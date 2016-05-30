package examples

import scala.language.implicitConversions

sealed trait ExtractedValue[T]

class Args2[T](desired: Manifest[T]) {
  def unapply[X](c: X)(implicit m: Manifest[X]): Option[T] = {
    def sameArgs = desired.typeArguments.zip(m.typeArguments).forall { case (d, actual) => d >:> actual }
    if (desired >:> m && sameArgs) Some(c.asInstanceOf[T])
    else None
  }
}

case class Extracted[T](t: T) extends ExtractedValue[T]

case class Missing[T]() extends ExtractedValue[T]

abstract class Bob[A](desired: Manifest[A]) extends Function[PartialFunction[A, String], Function[A, String]] {
  val Args = new Args2[A](desired)
}

class Magnet[A] {
  def apply(desired: Manifest[A]): Bob[A] = new Bob[A](desired) {
    override def apply(pf: PartialFunction[A, String]): Function[A, String] = (in: A) => pf(in)
  }
}

object Magnet {

  def apply[A](magnet: Magnet[A])(implicit desired: Manifest[A]) = magnet.apply(desired)

  implicit def t2ToMagnet[A, B](a: (A, B)): Magnet[(A, B)] = new Magnet[(A, B)]

  implicit def t3ToMagnet[A, B, C](a: (A, B, C)): Magnet[(A, B, C)] = new Magnet[(A, B, C)]
}

object MagnetApp extends App {

  private val it1 = Magnet(1, 3)
  private val out = it1 {
    case it1.Args(a, b) => "asd " + a + b
  }

  println(out(1, 2))

}
