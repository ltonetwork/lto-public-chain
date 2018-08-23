package one.legalthings.mining

import one.legalthings.state.Blockchain
import one.legalthings.transaction.assets.exchange.ExchangeTransaction
import one.legalthings.transaction.assets.{BurnTransaction, ReissueTransaction, SponsorFeeTransaction}
import one.legalthings.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import one.legalthings.transaction.{Authorized, Transaction}

object TxEstimators {
  abstract class Fn extends ((Blockchain, Transaction) => Long) {
    val minEstimate: Long
  }

  object sizeInBytes extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = x.bytes().length // + headers

    override def toString(): String = "sizeInBytes"

    override val minEstimate = 109l
  }

  object one extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = 1

    override def toString(): String = "one"

    override val minEstimate = 1l
  }

  object scriptRunNumber extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = {
      val smartAccountRun = x match {
        case x: Transaction with Authorized if blockchain.hasScript(x.sender) => 1
        case _                                                                => 0
      }

      smartAccountRun
    }

    override def toString(): String = "scriptRunNumber"

    override val minEstimate = 0l
  }
}
