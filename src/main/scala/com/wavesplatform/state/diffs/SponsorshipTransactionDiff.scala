package com.wavesplatform.state.diffs

import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.{SponsorshipCancelTransaction, SponsorshipTransaction, ValidationError}

object SponsorshipTransactionDiff {
  def sponsor(blockchain: Blockchain, height: Int)(tx: SponsorshipTransaction): Either[ValidationError, Diff] =
    blockchain.sponsorOf(tx.recipient) match {
      case Some(sponsor) => Left(GenericError(s"${tx.recipient} is already sponsored by $sponsor"))
      case None =>
        Right(
          Diff(height,
               tx,
               sponsoredBy = Map((tx.recipient -> (tx.sender, true)))))
    }
  def cancel(blockchain: Blockchain, height: Int)(tx: SponsorshipCancelTransaction): Either[ValidationError, Diff] =
    blockchain.sponsorOf(tx.recipient) match {
      case None => Left(GenericError(s"${tx.recipient} is not sponsored by anyone"))
      case Some(sponsor) =>
        if (tx.sender.toAddress != sponsor) Left(GenericError(s"${tx.recipient} is sponsored by $sponsor"))
        else
          Right(
            Diff(height,
                 tx,
                 sponsoredBy = Map((tx.recipient -> (tx.sender, false)))))
    }
}
