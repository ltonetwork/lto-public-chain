package com.ltonetwork.transaction.smart

import com.ltonetwork.lang.v1.traits.{Environment, Tx => ContractTransaction}
import com.ltonetwork.state._
import com.ltonetwork.transaction.Transaction
import monix.eval.Coeval

class LtoEnvironment(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain) extends Environment {
  override def height: Int                      = h()
  override def transaction: ContractTransaction = RealTransactionWrapper(tx())
  override def networkByte: Byte                = nByte
}
