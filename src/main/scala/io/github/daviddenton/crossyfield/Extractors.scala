package io.github.daviddenton.crossyfield

/**
  * Convenience Extractors
  */
object Extractors {

  /**
    * Converting to various primitive types from a String
    */
  object string {
    val optional = new PrimitiveExtractors(false)
    val required = new PrimitiveExtractors(true)
  }
}
