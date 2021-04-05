package com.ltonetwork.state

import com.ltonetwork.block.Block.BlockId
import com.ltonetwork.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
