package com.wavesplatform.state.diffs

import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.{SponsorshipCancelTransaction, SponsorshipTransaction, ValidationError}

object SponsorshipTransactionDiff {
  def sponsor(height: Int)(tx: SponsorshipTransaction) : Either[ValidationError, Diff] = ???
  def cancel(height: Int)(tx: SponsorshipCancelTransaction) : Either[ValidationError, Diff] = ???
}
