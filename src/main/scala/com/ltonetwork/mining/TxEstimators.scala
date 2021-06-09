package com.ltonetwork.mining

import com.ltonetwork.state.Blockchain
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.genesis.GenesisTransaction

object TxEstimators {
  abstract class Fn extends ((Blockchain, Transaction) => Long) {
    val minEstimate: Long
  }

  object sizeInBytes extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = x.bytes().length // + headers

    override def toString(): String = "sizeInBytes"

    override val minEstimate = 109L
  }

  object one extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = 1

    override def toString(): String = "one"

    override val minEstimate = 1L
  }

  object scriptRunNumber extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = {
      val smartAccountRun = x match {
        case _: GenesisTransaction               => 0
        case _ if blockchain.hasScript(x.sender) => 1
        case _                                   => 0
      }

      smartAccountRun
    }

    override def toString(): String = "scriptRunNumber"

    override val minEstimate = 0L
  }
}
