package com.ltonetwork.state.diffs

import com.ltonetwork.account.Address
import com.ltonetwork.state.{Blockchain, Diff, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.statement.StatementTransaction

object StatementTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: StatementTransaction): Either[ValidationError, Diff] =
    Right(Diff(
      height,
      tx,
      Map(tx.sender.toAddress -> Portfolio.empty) ++
        tx.recipient.fold(Map.empty[Address, Portfolio])(r => Map(r -> Portfolio.empty))
    ))
}
