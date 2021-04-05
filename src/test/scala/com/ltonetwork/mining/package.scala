package com.ltonetwork

import com.ltonetwork.state.Blockchain
import com.ltonetwork.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long) = OneDimensionalMiningConstraint(
    maxSize,
    new com.ltonetwork.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction) = transactionSize
      override val minEstimate                          = transactionSize
    }
  )
}
