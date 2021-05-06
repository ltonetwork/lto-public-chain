package com.ltonetwork.generator

import com.ltonetwork.transaction.Transaction

trait TransactionGenerator extends Iterator[Iterator[Transaction]] {
  override val hasNext = true
}
