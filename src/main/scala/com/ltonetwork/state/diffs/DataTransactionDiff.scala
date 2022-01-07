package com.ltonetwork.state.diffs

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.state.{AccountDataInfo, Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.data.DataTransaction

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    (
      if (blockchain.isFeatureActivated(BlockchainFeatures.CobaltAlloy, height))
        for {
          _ <- Either.cond(tx.data.lengthCompare(DataTransaction.MaxEntryCount) <= 0,
            (),
            GenericError(s"Max amount of $DataTransaction.MaxEntryCount entries reached"))
          _ <- Either.cond(!tx.data.exists(_.key.isEmpty), (), GenericError("Empty key found"))
          _ <- Either.cond(tx.data.map(_.key).distinct.lengthCompare(tx.data.size) == 0,
            (),
            GenericError("Duplicate keys found"))
          _ <- Either.cond(tx.bytes().length <= DataTransaction.MaxBytes,
            (),
            GenericError(s"Max bytes of $DataTransaction.MaxBytes reached"))
        } yield ()
      else Right(())
    ).map(
      _ =>
        Diff(
          height,
          tx,
          accountData = Map(tx.sender.toAddress -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))
        )
    )
  }
}
