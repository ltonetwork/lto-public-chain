package com.ltonetwork.state.diffs

import com.ltonetwork.account.{KeyType, KeyTypes}
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.state.{Blockchain, Diff, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.register.RegisterTransaction

object RegisterTransactionDiff {

  import com.ltonetwork.features.FeatureProvider._

  def apply(blockchain: Blockchain, height: Int)(tx: RegisterTransaction): Either[ValidationError, Diff] = {
    val allowedKeyTypes: Set[KeyType] =
      if (blockchain.isFeatureActivated(BlockchainFeatures.Palladium, height))
        Set(KeyTypes.ED25519, KeyTypes.SECP256K1, KeyTypes.SECP256R1, KeyTypes.BLS12_381)
      else
        Set(KeyTypes.ED25519, KeyTypes.SECP256K1, KeyTypes.SECP256R1)

    val invalidKeyTypes = tx.accounts.filterNot(a => allowedKeyTypes.contains(a.keyType))

    if (invalidKeyTypes.nonEmpty)
      Left(GenericError("Invalid key type found in accounts"))
    else
      Right(Diff(
        height,
        tx,
        Map(tx.sender.toAddress -> Portfolio.empty)
      ))
  }
}
