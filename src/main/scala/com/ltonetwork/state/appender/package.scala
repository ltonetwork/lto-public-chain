package com.ltonetwork.state

import cats.implicits._
import com.ltonetwork.block.Block
import com.ltonetwork.block.Block.BlockId
import com.ltonetwork.consensus.{GeneratingBalanceProvider, PoSSelector}
import com.ltonetwork.mining._
import com.ltonetwork.network._
import com.ltonetwork.settings.LtoSettings
import com.ltonetwork.transaction.ValidationError.{BlockAppendError, BlockFromFuture, GenericError}
import com.ltonetwork.transaction._
import com.ltonetwork.utils.{ScorexLogging, Time}
import com.ltonetwork.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task

import scala.util.{Left, Right}

package object appender extends ScorexLogging {

  private val MaxTimeDrift: Long = 100 // millis

  // Invalid blocks, that are already in blockchain
  private val exceptions = List(
  )

  private[appender] def processAndBlacklistOnFailure[A, B](
      ch: Channel,
      peerDatabase: PeerDatabase,
      miner: Miner,
      allChannels: ChannelGroup,
      start: => String,
      success: => String,
      errorPrefix: String)(f: => Task[Either[B, Option[BigInt]]]): Task[Either[B, Option[BigInt]]] = {

    log.debug(start)
    f map {
      case Right(maybeNewScore) =>
        log.debug(success)
        maybeNewScore.foreach(_ => miner.scheduleMining())
        Right(maybeNewScore)
      case Left(ve) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ch, s"$errorPrefix: $ve")
        Left(ve)
    }
  }

  private[appender] def appendBlock(checkpoint: CheckpointService,
                                    blockchainUpdater: BlockchainUpdater with Blockchain,
                                    utxStorage: UtxPool,
                                    pos: PoSSelector,
                                    time: Time,
                                    settings: LtoSettings)(block: Block): Either[ValidationError, Option[Int]] =
    for {
      _ <- Either.cond(
        checkpoint.isBlockValid(block.signerData.signature, blockchainUpdater.height + 1),
        (),
        BlockAppendError(s"Block $block at height ${blockchainUpdater.height + 1} is not valid w.r.t. checkpoint", block)
      )
      _ <- Either.cond(
        !blockchainUpdater.hasScript(block.sender),
        (),
        BlockAppendError(s"Account(${block.sender.toAddress}) is scripted are therefore not allowed to forge blocks", block)
      )
      _ <- blockConsensusValidation(blockchainUpdater, settings, pos, time.correctedTime(), block) { (height, parent) =>
        val balance = GeneratingBalanceProvider.balance(blockchainUpdater, settings.blockchainSettings.functionalitySettings, block.sender, parent)
        Either.cond(
          GeneratingBalanceProvider.isEffectiveBalanceValid(blockchainUpdater,
                                                            settings.blockchainSettings.functionalitySettings,
                                                            height,
                                                            block,
                                                            balance),
          balance,
          s"generator's effective balance $balance is less that required for generation"
        )
      }
      baseHeight = blockchainUpdater.height
      maybeDiscardedTxs <- blockchainUpdater.processBlock(block)
    } yield {
      utxStorage.removeAll(block.transactionData)
      utxStorage.batched { ops =>
        maybeDiscardedTxs.toSeq.flatten.foreach(ops.putIfNew)
      }
      maybeDiscardedTxs.map(_ => baseHeight)
    }

  private def blockConsensusValidation(blockchain: Blockchain, settings: LtoSettings, pos: PoSSelector, currentTs: Long, block: Block)(
      genBalance: (Int, BlockId) => Either[String, Long]): Either[ValidationError, Unit] = {

    val blockTime = block.timestamp

    for {
      height <- blockchain.heightOf(block.reference).toRight(GenericError(s"height: history does not contain parent ${block.reference}"))
      parent <- blockchain.parent(block).toRight(GenericError(s"parent: history does not contain parent ${block.reference}"))
      grandParent = blockchain.parent(parent, 2)
      effectiveBalance <- genBalance(height, block.reference).left.map(GenericError(_))
      _                <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime))
      _                <- pos.validateBaseTarget(height, block, parent, grandParent)
      _                <- pos.validateGeneratorSignature(height, block)
      _                <- pos.validateBlockDelay(height, block, parent, effectiveBalance).orElse(checkExceptions(height, block))
    } yield ()
  }.left.map {
    case GenericError(x) => GenericError(s"Block $block is invalid: $x")
    case x               => x
  }

  private def checkExceptions(height: Int, block: Block): Either[ValidationError, Unit] = {
    Either
      .cond(
        exceptions.contains((height, block.uniqueId)),
        (),
        GenericError(s"Block time ${block.timestamp} less than expected")
      )
  }

}
