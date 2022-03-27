package com.ltonetwork.state.diffs

import cats.implicits._
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.transfer._

import scala.util.Right

object TransferTransactionDiff {
  private def transfer(height: Int, tx: TransferTransaction): Diff = {
    val sender     = tx.sender.toAddress
    val recipient  = tx.recipient

    val portfolios = Map(sender -> Portfolio(-tx.amount)).combine(Map(recipient -> Portfolio(tx.amount)))
    Diff(height, tx, portfolios = portfolios)
  }

  private def burn(height: Int, tx: TransferTransaction): Diff = {
    val sender     = tx.sender.toAddress

    val portfolios = Map(sender -> Portfolio(-tx.amount))
    Diff(height, tx, portfolios = portfolios, burned = tx.amount)
  }

  def apply(blockchain: Blockchain, s: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: TransferTransaction): Either[ValidationError, Diff] = {

    Right(
      if (s.burnAddresses.contains(tx.recipient.toString)) burn(height, tx) else transfer(height, tx)
    )
  }
}
