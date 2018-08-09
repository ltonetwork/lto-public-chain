package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.account.Address
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.transfer._

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
    } yield (portfolios, blockTime > s.allowUnissuedAssetsUntil)

    isInvalidEi match {
      case Left(e) => Left(e)
      case Right((portfolios, invalid)) =>
        if (invalid)
          Left(GenericError(s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${s.allowUnissuedAssetsUntil}"))
        else
          Right(Diff(height, tx, portfolios))
    }
  }
}
