package com.ltonetwork.state.diffs

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.anchor.AnchorTransactionV1

object AnchorTransactionDiff {
  import com.ltonetwork.features.FeatureProvider._
  def apply(blockchain: Blockchain, height: Int)(tx: AnchorTransaction): Either[ValidationError, Diff] = {
    (if (blockchain.isFeatureActivated(BlockchainFeatures.AssociationTransaction, height))
       for {
         _ <- Either.cond(tx.anchors.size == 1, (), GenericError("AnchorTransaction should have exactly 1 anchor"))
         _ <- Either.cond(tx.anchors.head.arr.length <= AnchorTransactionV1.NewMaxEntryLength, (), GenericError("Anchor should contain <= 64 bytes"))
       } yield ()
     else Right(())).map(
      _ =>
        Diff(
          height,
          tx,
          accountData = Map.empty
      ))
  }
}
