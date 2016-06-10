package examples

import io.github.daviddenton.crossyfield.{Extracted, Extractors, Invalid, NotProvided}

case class T(a: Option[Int], b: Option[Int])

object MagnetApp extends App {

  private val int = Extractors.string.optional.int('asd)
  private val bbb = Extractors.string.optional.int('asd2)

  val extracted = Validation.mk(
    int <--? "",
    bbb <--? ""
  ) {
    case (a, b) => "as " + a + b
  }

  extracted match {
    case Extracted(b) => println(b)
    case NotProvided => println("nothing")
    case Invalid(i) => println(i)
  }

}
