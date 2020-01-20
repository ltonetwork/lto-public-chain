package com.wavesplatform.state.diffs

import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.{AssociationTransactionBase, ValidationError}

object AssociationTransactionDiff {

  def apply(height: Int)(tx: AssociationTransactionBase): Either[ValidationError, Diff] =
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty)),
        accountData = Map.empty,
      ))
}
