package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Diff, LeaseBalance, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.association.IssueAssociationTransaction

object AssociationTransactionDiff {

  def apply(height: Int)(tx: IssueAssociationTransaction): Either[ValidationError, Diff] =
    Right(
      Diff(
        height,
        tx,
        accountData = Map.empty,
      ))
}
