package one.legalthings.state.diffs

import one.legalthings.features.BlockchainFeatures
import one.legalthings.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import one.legalthings.transaction.ValidationError.GenericError
import one.legalthings.transaction.{CreateAliasTransaction, ValidationError}
import one.legalthings.features.FeatureProvider._

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else
      Right(
        Diff(height = height,
             tx = tx,
             portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty)),
             aliases = Map(tx.alias               -> tx.sender.toAddress)))
}
