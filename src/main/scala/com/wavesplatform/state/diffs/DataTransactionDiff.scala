package com.wavesplatform.state.diffs

import com.wavesplatform.state._
import com.wavesplatform.transaction.{DataTransaction, ValidationError}

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] =
    Right(
      Diff(
        height,
        tx,
        accountData = Map(tx.sender.toAddress -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))
      ))
}
