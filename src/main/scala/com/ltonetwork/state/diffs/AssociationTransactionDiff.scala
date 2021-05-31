package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Diff, LeaseBalance, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.association.AssociationTransactionBase

object AssociationTransactionDiff {

  def apply(height: Int)(tx: AssociationTransactionBase): Either[ValidationError, Diff] =
    Right(
      Diff(
        height,
        tx,
        accountData = Map.empty,
      ))
}
