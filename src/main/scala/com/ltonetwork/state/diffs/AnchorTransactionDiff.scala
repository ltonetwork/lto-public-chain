package com.ltonetwork.state.diffs

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.anchor.AnchorTransaction

object AnchorTransactionDiff {
  import com.ltonetwork.features.FeatureProvider._

  val EntryLength: List[Int] = List(16, 20, 32, 48, 64)

  private def validateEntryLengths(tx: AnchorTransaction)  =
    Either.cond(
      tx.anchors.forall(a => EntryLength.contains(a.arr.length)),
      (),
      ValidationError.GenericError(s"Anchor can only be of length $EntryLength Bytes")
    )

  def apply(blockchain: Blockchain, height: Int)(tx: AnchorTransaction): Either[ValidationError, Diff] = {
    (
      if (blockchain.isFeatureActivated(BlockchainFeatures.Titanium, height)) {
        Right(())
      } else if (
        !blockchain.isFeatureActivated(BlockchainFeatures.Cobalt, height) &&
        blockchain.isFeatureActivated(BlockchainFeatures.AssociationTransaction, height)
      ) {
        for {
          _ <- Either.cond(tx.anchors.size == 1, (), GenericError("AnchorTransaction should have exactly 1 anchor"))
          _ <- validateEntryLengths(tx)
        } yield ()
      } else {
        validateEntryLengths(tx)
      }
    ).map(
      _ =>
        Diff(
          height,
          tx,
          accountData = Map.empty
      )
    )
  }
}
