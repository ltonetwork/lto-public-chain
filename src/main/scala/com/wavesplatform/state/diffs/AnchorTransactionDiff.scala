package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.{AnchorTransaction, ValidationError}

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
