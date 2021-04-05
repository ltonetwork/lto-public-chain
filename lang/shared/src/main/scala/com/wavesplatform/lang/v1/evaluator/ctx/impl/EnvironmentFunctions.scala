package com.wavesplatform.lang.v1.evaluator.ctx.impl

object EnvironmentFunctions {
  val ChecksumLength       = 4
  val HashLength           = 20
  val AddressVersion: Byte = 1
  val AddressLength: Int   = 1 + 1 + ChecksumLength + HashLength
  val AddressPrefix        = "address:"
}
