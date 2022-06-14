package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Diff, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.smart.SetScriptTransaction

object SetScriptTransactionDiff {
  def apply(height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio.empty),
        scripts = Map(tx.sender.toAddress -> tx.script)
      ))
  }
}
