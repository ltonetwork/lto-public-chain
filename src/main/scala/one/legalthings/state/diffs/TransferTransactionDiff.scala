package one.legalthings.state.diffs

import cats.implicits._
import one.legalthings.settings.FunctionalitySettings
import one.legalthings.state._
import one.legalthings.account.Address
import one.legalthings.transaction.ValidationError
import one.legalthings.transaction.ValidationError.GenericError
import one.legalthings.transaction.transfer._

import scala.util.Right

object TransferTransactionDiff {
  def apply(blockchain: Blockchain, s: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: TransferTransaction): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.sender.publicKey)

    val isInvalidEi = for {
      recipient <- blockchain.resolveAlias(tx.recipient)
      portfolios = Map(sender -> Portfolio(-tx.amount, LeaseBalance.empty))
        .combine(
          Map(recipient -> Portfolio(tx.amount, LeaseBalance.empty))
        )
        .combine(
          Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty))
        )
    } yield portfolios

    isInvalidEi match {
      case Left(e) => Left(e)
      case Right(portfolios) =>
        Right(Diff(height, tx, portfolios))
    }
  }
}
