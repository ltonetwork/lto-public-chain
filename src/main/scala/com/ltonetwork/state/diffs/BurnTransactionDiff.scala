package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Blockchain, Diff, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.burn.BurnTransaction

object BurnTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    Right(Diff(height, tx, portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.amount)), burned = tx.amount))
  }
}
