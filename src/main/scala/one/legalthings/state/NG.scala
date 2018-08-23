package one.legalthings.state

import one.legalthings.block.Block.BlockId
import one.legalthings.block.MicroBlock

trait NG extends Blockchain {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]

  def lastPersistedBlockIds(count: Int): Seq[BlockId]

  def microblockIds: Seq[BlockId]
}
