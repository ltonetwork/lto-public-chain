package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Diff, LeaseBalance, Portfolio}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.genesis.GenesisTransaction

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx, portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount))))
  }
}
