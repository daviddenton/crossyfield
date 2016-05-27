package io.github.daviddenton.crossyfield

object Validators {
  object string {
    val optional = new PrimitiveValidators(false)
    val required = new PrimitiveValidators(true)
  }
}
