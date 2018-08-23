package one.legalthings.state.diffs

import one.legalthings.state.{Diff, LeaseBalance, Portfolio}
import one.legalthings.transaction.ValidationError
import one.legalthings.transaction.smart.SetScriptTransaction

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty)),
        scripts = Map(tx.sender.toAddress    -> tx.script)
      ))
  }
}
