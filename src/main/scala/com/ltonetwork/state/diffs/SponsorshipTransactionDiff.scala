package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.{SponsorshipCancelTransaction, SponsorshipTransaction, ValidationError}

object SponsorshipTransactionDiff {
  def sponsor(blockchain: Blockchain, height: Int)(tx: SponsorshipTransaction): Either[ValidationError, Diff] = {

    val list       = blockchain.sponsorOf(tx.recipient)
    val newSponsor = tx.sender.toAddress
    if (list.isEmpty)
      Right(Diff(height, tx, sponsoredBy = Map((tx.recipient -> List(newSponsor)))))
    else if (list.contains(newSponsor))
      Left(GenericError(s"${tx.recipient} is already sponsored by ${newSponsor}")) // TODO consider reordering to increase priority
    else
      Right(Diff(height, tx, sponsoredBy = Map((tx.recipient -> (newSponsor +: list)))))
  }

  def cancel(blockchain: Blockchain, height: Int)(tx: SponsorshipCancelTransaction): Either[ValidationError, Diff] = {
    val list          = blockchain.sponsorOf(tx.recipient)
    val formerSponsor = tx.sender.toAddress
    if (!list.contains(formerSponsor)) Left(GenericError(s"${tx.recipient} is not sponsored by $formerSponsor"))
    else Right(Diff(height, tx, sponsoredBy = Map((tx.recipient -> list.filterNot(_ == formerSponsor)))))
  }
}
