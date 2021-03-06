package com.ltonetwork.consensus

import com.ltonetwork.transaction.Transaction

object TransactionsOrdering {
  trait LtoOrdering extends Ordering[Transaction] {
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Long, Long, String) = {
      val byFee       = -t.fee
      val byTimestamp = txTimestampOrder(t.timestamp)
      val byTxId      = t.id().base58

      (byFee, byTimestamp, byTxId)
    }
    override def compare(first: Transaction, second: Transaction): Int = {
      implicitly[Ordering[(Long, Long, String)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends LtoOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long = -ts
  }

  object InUTXPool extends LtoOrdering {
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
