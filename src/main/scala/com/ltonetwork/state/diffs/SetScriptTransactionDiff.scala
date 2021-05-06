package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Diff, LeaseBalance, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.smart.SetScriptTransaction

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        scripts = Map(tx.sender.toAddress -> tx.script)
      ))
  }
}
