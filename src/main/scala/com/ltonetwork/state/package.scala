package com.ltonetwork

import com.ltonetwork.account.Address
import com.ltonetwork.block.Block
import com.ltonetwork.block.Block.BlockId
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import com.ltonetwork.utils.ScorexLogging

import scala.reflect.ClassTag
import scala.util.Try

package object state {
  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  implicit class EitherExt[L <: ValidationError, R](ei: Either[L, R]) {
    def liftValidationError[T <: Transaction](t: T): Either[ValidationError, R] = {
      ei.left.map(e => GenericError(e.toString))
    }
  }

  implicit class EitherExt2[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value)  => throw new Exception(value.toString)
      case Right(value) => value
    }
  }

  implicit class Cast[A](a: A) {
    def cast[B: ClassTag]: Option[B] = {
      a match {
        case b: B => Some(b)
        case _    => None
      }
    }
  }

  implicit class BlockchainExt(blockchain: Blockchain) extends ScorexLogging {
    def assocExists(as: AssociationTransactionBase) =
      blockchain.associations(as.sender).outgoing.map(_._2).exists(_.assoc == as.assoc)

    def isEmpty: Boolean = blockchain.height == 0

    def contains(block: Block): Boolean       = blockchain.contains(block.uniqueId)
    def contains(signature: ByteStr): Boolean = blockchain.heightOf(signature).isDefined

    def blockById(blockId: ByteStr): Option[Block] = blockchain.blockBytes(blockId).flatMap(bb => Block.parseBytes(bb).toOption)
    def blockAt(height: Int): Option[Block]        = blockchain.blockBytes(height).flatMap(bb => Block.parseBytes(bb).toOption)

    def lastBlockHeaderAndSize: Option[(Block, Int)] = blockchain.lastBlock.map(b => (b, b.bytes().length))
    def lastBlockId: Option[ByteStr]                 = blockchain.lastBlockHeaderAndSize.map(_._1.uniqueId)
    def lastBlockTimestamp: Option[Long]             = blockchain.lastBlockHeaderAndSize.map(_._1.timestamp)

    def lastBlocks(howMany: Int): Seq[Block] = {
      (Math.max(1, blockchain.height - howMany + 1) to blockchain.height).flatMap(blockchain.blockAt).reverse
    }

    def genesis: Block = blockchain.blockAt(1).get

    def effectiveBalance(address: Address, confirmations: Int, block: BlockId = blockchain.lastBlockId.getOrElse(ByteStr.empty)): Long = {
      val blockHeight = blockchain.heightOf(block).getOrElse(blockchain.height)
      val bottomLimit = (blockHeight - confirmations + 1).max(1).min(blockHeight)
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, block)
      if (balances.isEmpty) 0L else balances.view.map(_.effectiveBalance).min
    }

    def balance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val block       = blockchain.blockAt(atHeight).getOrElse(throw new IllegalArgumentException(s"Invalid block height: $atHeight"))
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, block.uniqueId)
      if (balances.isEmpty) 0L else balances.view.map(_.regularBalance).min
    }

    def activeLeases(address: Address): Seq[(Int, LeaseTransaction)] =
      blockchain
        .addressTransactions(address, Set(LeaseTransactionV1.typeId), Int.MaxValue, 0)
        .collect { case (h, l: LeaseTransaction) if blockchain.leaseDetails(l.id()).exists(_.isActive) => h -> l }

    def unsafeHeightOf(id: ByteStr): Int =
      blockchain
        .heightOf(id)
        .getOrElse(throw new IllegalStateException(s"Can't find a block: $id"))

  }

}