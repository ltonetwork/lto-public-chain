package com.wavesplatform.state.diffs

import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.{SponsorshipCancelTransaction, SponsorshipTransaction, ValidationError}

object SponsorshipTransactionDiff {
  def sponsor(blockchain: Blockchain, height: Int)(tx: SponsorshipTransaction) : Either[ValidationError, Diff] = ???
  def cancel(blockchain: Blockchain, height: Int)(tx: SponsorshipCancelTransaction) : Either[ValidationError, Diff] = ???
}
