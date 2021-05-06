package com.ltonetwork.state.appender

import cats.data.EitherT
import com.ltonetwork.metrics.{BlockStats, Instrumented}
import com.ltonetwork.network.MicroBlockSynchronizer.MicroblockData
import com.ltonetwork.network._
import com.ltonetwork.state.Blockchain
import com.ltonetwork.utils.ScorexLogging
import com.ltonetwork.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import com.ltonetwork.block.MicroBlock
import com.ltonetwork.transaction.ValidationError.{InvalidSignature, MicroBlockAppendError}
import com.ltonetwork.transaction.{BlockchainUpdater, CheckpointService, ValidationError}

import scala.util.{Left, Right}

object MicroblockAppender extends ScorexLogging with Instrumented {

  def apply(checkpoint: CheckpointService, blockchainUpdater: BlockchainUpdater with Blockchain, utxStorage: UtxPool, scheduler: Scheduler)(
      microBlock: MicroBlock): Task[Either[ValidationError, Unit]] =
    Task(
      measureSuccessful(
        microblockProcessingTimeStats,
        for {
          _ <- Either.cond(
            checkpoint.isBlockValid(microBlock.totalResBlockSig, blockchainUpdater.height + 1),
            (),
            MicroBlockAppendError(s"[h = ${blockchainUpdater.height + 1}] is not valid with respect to checkpoint", microBlock)
          )
          _ <- blockchainUpdater.processMicroBlock(microBlock)
        } yield utxStorage.removeAll(microBlock.transactionData)
      )).executeOn(scheduler)

  def apply(checkpoint: CheckpointService,
            blockchainUpdater: BlockchainUpdater with Blockchain,
            utxStorage: UtxPool,
            allChannels: ChannelGroup,
            peerDatabase: PeerDatabase,
            scheduler: Scheduler)(ch: Channel, md: MicroblockData): Task[Unit] = {
    import md.microBlock
    val microblockTotalResBlockSig = microBlock.totalResBlockSig
    (for {
      _                <- EitherT(Task.now(microBlock.signaturesValid()))
      validApplication <- EitherT(apply(checkpoint, blockchainUpdater, utxStorage, scheduler)(microBlock))
    } yield validApplication).value.map {
      case Right(()) =>
        md.invOpt match {
          case Some(mi) => allChannels.broadcast(mi, except = md.microblockOwners())
          case None     => log.warn(s"${id(ch)} Not broadcasting MicroBlockInv")
        }
        BlockStats.applied(microBlock)
      case Left(is: InvalidSignature) =>
        peerDatabase.blacklistAndClose(ch, s"Could not append microblock $microblockTotalResBlockSig: $is")
      case Left(ve) =>
        BlockStats.declined(microBlock)
        log.debug(s"${id(ch)} Could not append microblock $microblockTotalResBlockSig: $ve")
    }
  }

  private val microblockProcessingTimeStats = Kamon.metrics.histogram("microblock-processing-time")
}
