package one.legalthings.transaction

import one.legalthings.state.ByteStr
import monix.reactive.Observable
import one.legalthings.block.Block.BlockId
import one.legalthings.block.{Block, MicroBlock}

trait BlockchainUpdater {
  def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]]

  def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]

  def lastBlockInfo: Observable[LastBlockInfo]

  def isLastBlockId(id: ByteStr): Boolean

  def shutdown(): Unit
}

case class LastBlockInfo(id: BlockId, height: Int, score: BigInt, ready: Boolean)
