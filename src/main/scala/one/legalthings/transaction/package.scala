package one.legalthings

import one.legalthings.utils.base58Length
import one.legalthings.block.{Block, MicroBlock}

package object transaction {

  type AssetId = one.legalthings.state.ByteStr
  val AssetIdLength: Int       = one.legalthings.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
