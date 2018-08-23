package one.legalthings.state.appender

import one.legalthings.mining.Miner
import one.legalthings.network.{BlockCheckpoint, Checkpoint, PeerDatabase, id}
import one.legalthings.state.{Blockchain, ByteStr}
import one.legalthings.utils.ScorexLogging
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import one.legalthings.transaction.{BlockchainUpdater, CheckpointService, ValidationError}

object CheckpointAppender extends ScorexLogging {
  def apply(checkpointService: CheckpointService,
            blockchain: Blockchain,
            blockchainUpdater: BlockchainUpdater,
            peerDatabase: PeerDatabase,
            miner: Miner,
            allChannels: ChannelGroup,
            scheduler: Scheduler)(maybeChannel: Option[Channel], c: Checkpoint): Task[Either[ValidationError, Option[BigInt]]] = {
    val t = Task(checkpointService.set(c).map { _ =>
      log.info(s"Processing checkpoint $c")
      makeBlockchainCompliantWith(blockchain, blockchainUpdater)(c)
      blockchain.score
    }).executeOn(scheduler).map(_.map(Some(_)))
    maybeChannel match {
      case None => t
      case Some(ch) =>
        processAndBlacklistOnFailure(
          ch,
          peerDatabase,
          miner,
          allChannels,
          s"${id(ch)} Attempting to process checkpoint",
          s"${id(ch)} Successfully processed checkpoint",
          s"${id(ch)} Error processing checkpoint"
        )(t)
    }
  }

  private def makeBlockchainCompliantWith(blockchain: Blockchain, blockchainUpdater: BlockchainUpdater)(checkpoint: Checkpoint): Unit = {
    val existingItems = checkpoint.items.filter { checkpoint =>
      blockchain.blockAt(checkpoint.height).isDefined
    }

    val fork = existingItems.takeWhile {
      case BlockCheckpoint(h, sig) =>
        val block = blockchain.blockAt(h).get
        block.signerData.signature != ByteStr(sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh                 = existingItems.map(_.height) :+ genesisBlockHeight
      blockchain.blockAt(hh(fork.size)).foreach { lastValidBlock =>
        log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block $lastValidBlock]")
        blockBlockForkStats.increment()
        blockForkHeightStats.record(fork.size)
        blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  private val blockBlockForkStats  = Kamon.metrics.counter("block-fork")
  private val blockForkHeightStats = Kamon.metrics.histogram("block-fork-height")
}
