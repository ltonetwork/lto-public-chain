package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.anchor.MappedAnchorTransaction

object MappedAnchorTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: MappedAnchorTransaction): Either[ValidationError, Diff] =
    Right(Diff(height, tx))
}