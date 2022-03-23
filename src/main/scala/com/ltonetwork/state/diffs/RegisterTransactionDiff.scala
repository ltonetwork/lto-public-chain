package com.ltonetwork.state.diffs

import com.ltonetwork.state.{Blockchain, Diff}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.register.RegisterTransaction

object RegisterTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: RegisterTransaction): Either[ValidationError, Diff] =
    Right(Diff(height, tx))
}