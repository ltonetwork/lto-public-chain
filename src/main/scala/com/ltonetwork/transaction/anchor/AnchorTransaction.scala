package com.ltonetwork.transaction.anchor

import com.ltonetwork.state._
import com.ltonetwork.transaction._

trait AnchorTransaction extends ProvenTransaction with VersionedTransaction with FastHashId {
  def version: Byte
  def timestamp: Long
  def fee: Long
  def anchors: List[ByteStr]
}

