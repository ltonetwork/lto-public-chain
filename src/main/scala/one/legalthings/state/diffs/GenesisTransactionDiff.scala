package one.legalthings.state.diffs

import one.legalthings.state.{Diff, LeaseBalance, Portfolio}
import one.legalthings.transaction.ValidationError.GenericError
import one.legalthings.transaction.{GenesisTransaction, ValidationError}

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx, portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount, LeaseBalance.empty))))
  }
}
