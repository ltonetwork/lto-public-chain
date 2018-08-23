package one.legalthings.state

import one.legalthings.block.Block.BlockId
import one.legalthings.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
