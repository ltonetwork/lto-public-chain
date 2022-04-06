package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.smart.SetScriptTransaction

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        scripts = Map(tx.sender.toAddress -> tx.script)
      ))
  }
}
