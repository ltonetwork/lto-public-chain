package com.ltonetwork.state.diffs

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.association.AssociationTransaction

object AssociationTransactionDiff {
  def oldMaxSubjectLength: Int = 64

  import com.ltonetwork.features.FeatureProvider._

  def apply(blockchain: Blockchain, height: Int)(tx: AssociationTransaction): Either[ValidationError, Diff] =
    (
      if (!blockchain.isFeatureActivated(BlockchainFeatures.Titanium, height))
        for {
          _ <- Either.cond(tx.subject.exists(_.length > oldMaxSubjectLength), (), GenericError("Subject should contain <= 64 bytes"))
        } yield ()
      else Right(())
    ).map(
      _ => Diff(
        height,
        tx,
      )
    )
}
