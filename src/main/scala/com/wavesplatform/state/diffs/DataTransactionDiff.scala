package com.wavesplatform.state.diffs

import com.wavesplatform.state._
import com.wavesplatform.transaction.{AnchorTransaction, AssociationTransaction, DataTransaction, ValidationError}

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(sender  -> Portfolio(-tx.fee, LeaseBalance.empty)),
        accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))
      ))
  }
}
object AnchorTransactionDiff {

  def apply(height: Int)(tx: AnchorTransaction): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty)),
        accountData = Map.empty
      ))
  }
}

object AssociationTransactionDiff {

  def apply(height: Int)(tx: AssociationTransaction): Either[ValidationError, Diff] =
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty)),
        accountData = Map.empty,
      ))
}
