package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform._
import com.wavesplatform.state._
import com.wavesplatform.account.Address
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.{GenericError, Validation}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._

object MassTransferTransactionDiff {

  def apply(blockchain: Blockchain, blockTime: Long, height: Int)(tx: MassTransferTransaction): Either[ValidationError, Diff] = {
    def parseTransfer(xfer: ParsedTransfer): (Map[Address, Portfolio], Long) = {
      val recipientAddr = xfer.address.asInstanceOf[Address]
      val portfolio     = Map(recipientAddr -> Portfolio(xfer.amount, LeaseBalance.empty))
      (portfolio, xfer.amount)
    }
    val portfoliosEi: Seq[(Map[Address, Portfolio], Long)] = tx.transfers.map(parseTransfer)

    val sender   = Address.fromPublicKey(tx.sender.publicKey)
    val (recipientPortfolios, totalAmount) = portfoliosEi.fold((Map.empty[Address, Portfolio], 0L)) { (u, v) =>
      (u._1 combine v._1, u._2 + v._2)
    }
    val completePortfolio = recipientPortfolios.combine(Map(sender -> Portfolio(-totalAmount, LeaseBalance.empty)))

    Right(Diff(height, tx, completePortfolio))
  }
}
