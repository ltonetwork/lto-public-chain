package one.legalthings.consensus.nxt

import one.legalthings.state.ByteStr

case class NxtLikeConsensusBlockData(baseTarget: Long, generationSignature: ByteStr)
