package com.wavesplatform.transaction.smart

import com.wavesplatform.lang.v1.traits.{Environment, Tx => ContractTransaction}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Transaction
import monix.eval.Coeval

class WavesEnvironment(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain) extends Environment {
  override def height: Int                      = h()
  override def transaction: ContractTransaction = RealTransactionWrapper(tx())
  override def networkByte: Byte                = nByte
}
