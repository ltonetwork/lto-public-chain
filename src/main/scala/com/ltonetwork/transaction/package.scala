package com.ltonetwork

import com.ltonetwork.utils.base58Length
import com.ltonetwork.block.{Block, MicroBlock}

package object transaction {

  type AssetId = com.ltonetwork.state.ByteStr
  val AssetIdLength: Int       = com.ltonetwork.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
