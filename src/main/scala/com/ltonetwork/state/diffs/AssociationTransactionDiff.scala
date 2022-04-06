package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.association.AssociationTransaction

object AssociationTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: AssociationTransaction): Either[ValidationError, Diff] =
    Right(Diff(height, tx))
}
