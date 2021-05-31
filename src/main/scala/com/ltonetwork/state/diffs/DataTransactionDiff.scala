package com.ltonetwork.state.diffs

import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.data.DataTransaction

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] =
    Right(
      Diff(
        height,
        tx,
        accountData = Map(tx.sender.toAddress -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))
      ))
}
