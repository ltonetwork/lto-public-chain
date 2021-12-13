package com.ltonetwork.state.diffs

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.register.RegisterTransaction

object RegisterTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: RegisterTransaction): Either[ValidationError, Diff] = {
    (
      if (blockchain.isFeatureActivated(BlockchainFeatures.Cobalt, height))
        for {
          _ <- Either.cond(tx.keys.forall(k => k.publicKey.length == k.keyType.length), (), GenericError("Invalid key length on one or more of the provided keys"))
          _ <- Either.cond(tx.keys.lengthCompare(RegisterTransaction.MaxEntryCount) <= 0, (), GenericError(s"Keys count should be <= $RegisterTransaction.MaxEntryCount"))
          _ <- Either.cond(tx.keys.map(_.publicKey).distinct.lengthCompare(tx.keys.size) == 0, (), GenericError("Duplicate key(s) found"))
        } yield ()
      else Right(())
    ).map(_ => Diff(height, tx))
  }
}
