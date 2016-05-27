package io.github.daviddenton.crossyfield

/**
  * Convienience Validators
  */
object Validators {

  /**
    * Converting to various primitive types from a String
    */
  object string {
    val optional = new PrimitiveValidators(false)
    val required = new PrimitiveValidators(true)
  }
}
