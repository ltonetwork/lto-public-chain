package com.wavesplatform.lang

import com.wavesplatform.lang.v1.traits.{Environment, Tx}

object WavesContext {
  val env = new Environment {
    override def height: Int                                         = impl.Environment.height
    override def networkByte: Byte                                   = impl.Environment.networkByte
    override def transaction: Tx                                     = impl.Environment.transaction
  }
}
