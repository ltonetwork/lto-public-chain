package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state._
import com.wavesplatform.transaction.ValidationError.GenericError
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
  import com.wavesplatform.features.FeatureProvider._
  def apply(blockchain: Blockchain, height: Int)(tx: AnchorTransaction): Either[ValidationError, Diff] = {
    (if (blockchain.isFeatureActivated(BlockchainFeatures.AssociationTransaction, height))
      for {
        _ <- Either.cond(tx.anchors.size == 1, (), GenericError("AnchorTransaction should have exactly 1 anchor"))
        _ <- Either.cond(tx.anchors.head.arr.length == AnchorTransaction.NewEntryLength, (), GenericError("Anchor should contain exactly 64 bytes"))
      } yield ()
    else Right(())).map(
      _ =>
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
