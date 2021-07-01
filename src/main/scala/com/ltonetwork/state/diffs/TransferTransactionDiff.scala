package com.ltonetwork.state.diffs

import cats.implicits._
import com.ltonetwork.account.Address
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.transfer._

import scala.util.Right

object TransferTransactionDiff {
  def apply(blockchain: Blockchain, s: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: TransferTransaction): Either[ValidationError, Diff] = {
    val sender     = tx.sender.toAddress
    val recipient  = tx.recipient
    val portfolios = Map(sender -> Portfolio(-tx.amount)).combine(Map(recipient -> Portfolio(tx.amount)))

    Right(Diff(height, tx, portfolios = portfolios))
  }
}
