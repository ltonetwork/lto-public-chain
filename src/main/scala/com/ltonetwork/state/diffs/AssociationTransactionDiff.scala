package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Diff, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.association.AssociationTransaction

object AssociationTransactionDiff {

  def apply(height: Int)(tx: AssociationTransaction): Either[ValidationError, Diff] =
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio.empty)
      ))
}
