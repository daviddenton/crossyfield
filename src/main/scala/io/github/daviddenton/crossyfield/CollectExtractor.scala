package io.github.daviddenton.crossyfield

import scala.language.implicitConversions

//case class CollectExtractionError()
//
//trait CollectExtractor[-From, +T] {
//  val identifier: Symbol
//
//  def <--?(from: From): CollectExtraction[T]
//
//  final def extract(from: From): CollectExtraction[T] = <--?(from)
//
//  final def <--?(from: From, error: String, predicate: T => Boolean): CollectExtraction[T] =
//    <--?(from).flatMap[T](v => if (v.map(predicate).getOrElse(true)) CollectExtraction(v) else CollectInvalid(identifier -> error))
//
//  final def extract(from: From, reason: String, predicate: T => Boolean): CollectExtraction[T] = <--?(from, reason, predicate)
//}
//
//object CollectExtractor {
//
//  def mk[From, T](id: Symbol)(fn: From => CollectExtraction[T]): Extractor[From, T] = new Extractor[From, T] {
//    override val identifier = id
//
//    override def <--?(from: From): CollectExtraction[T] = fn(from)
//  }
//
//  def mk[From, T](id: Symbol, message: String, fn: From => T): Extractor[From, T] = new Extractor[From, T] {
//    override val identifier: Symbol = id
//
//    override def <--?(from: From): CollectExtraction[T] = Try(fn(from)) match {
//      case Success(value) => CollectExtracted(value)
//      case Failure(e) => CollectInvalid(identifier -> message)
//    }
//  }
//}
//
//sealed trait CollectExtraction[+T] {
//  def flatMap[O](f: Option[T] => CollectExtraction[O]): CollectExtraction[O]
//
//  def map[O](f: Option[T] => O): CollectExtraction[O]
//
//  def orDefault[O >: T](f: => O): CollectExtraction[O]
//}
//
//object CollectExtraction {
//
//  def collectErrors(extractions: CollectExtraction[_]*): Seq[(Symbol, String)] = <--?(extractions) match {
//    case CollectInvalid(ip) => ip
//    case _ => Nil
//  }
//
//  def <--?(extractions: Seq[CollectExtraction[_]]): CollectExtraction[Nothing] = {
//    val missingOrFailed = extractions.flatMap {
//      case CollectInvalid(ip) => ip
//      case _ => Nil
//    }
//    if (missingOrFailed.isEmpty) CollectNotProvided else CollectInvalid(missingOrFailed)
//  }
//
//  def apply[T](t: Option[T]): CollectExtraction[T] = t.map(CollectExtracted(_)).getOrElse(CollectNotProvided)
//
//  def flatten[T](extraction: CollectExtraction[Option[T]]): CollectExtraction[T] =
//    extraction match {
//      case CollectExtracted(opt) => opt.map(CollectExtracted(_)).getOrElse(CollectNotProvided)
//      case CollectNotProvided => CollectNotProvided
//      case CollectInvalid(ip) => CollectInvalid(ip)
//    }
//}
//
//case class CollectExtracted[T](value: T) extends CollectExtraction[T] {
//  def flatMap[O](f: Option[T] => CollectExtraction[O]) = f(Some(value))
//
//  override def map[O](f: Option[T] => O) = CollectExtracted(f(Some(value)))
//
//  override def orDefault[O >: T](f: => O): CollectExtraction[O] = this
//}
//
//object CollectNotProvided extends CollectExtraction[Nothing] {
//
//  override def toString = "CollectNotProvided"
//
//  def flatMap[O](f: Option[Nothing] => CollectExtraction[O]) = f(None)
//
//  override def map[O](f: Option[Nothing] => O) = CollectExtracted(f(None))
//
//  override def orDefault[T](f: => T): CollectExtraction[T] = CollectExtracted(f)
//}
//
//case class CollectInvalid(invalid: Seq[(Symbol, String)]) extends CollectExtraction[Nothing] {
//  def flatMap[O](f: Option[Nothing] => CollectExtraction[O]) = CollectInvalid(invalid)
//
//  override def map[O](f: Option[Nothing] => O) = CollectInvalid(invalid)
//
//  override def orDefault[T](f: => T): CollectExtraction[T] = this
//}
//
//object CollectInvalid {
//  def apply(p: (Symbol, String)): CollectInvalid = CollectInvalid(Seq(p))
//}

trait ExtMagnet[From, Result] {
  type In

  def apply(): Function[PartialFunction[In, Result], Function[From, Extraction[Result]]]
}

object ExtMagnet {
  implicit def tuple2[A, B, From, Result](tuple: (Extractor[From, A], Extractor[From, B])): ExtMagnet[From, Result] =
    new ExtMagnet[From, Result] {
      override type In = (Option[A], Option[B])

      override def apply() = pf => from => {

        Extraction.<--?(Seq(tuple._1 <--? from, tuple._2 <--? from)) match {
          case Invalid(ip) => Invalid(ip)
          case _ => Extracted(pf.apply(None, None))
        }
      }
    }
}

object Bob extends App {
  def composite[From, Int](extMagnet: ExtMagnet[From, Int]) = extMagnet.apply()

  val a = composite[String, Int](
    Extractors.string.required.int('bob),
    Extractors.string.required.int('bob2)
  ) {
    case (as: Option[Int], bs: Option[Int]) => as.flatMap(a => bs.map(b => a + b)).getOrElse(1)
  }
  println(a.apply("sdf"))
}