package com.ltonetwork.generator

import java.util.concurrent.ThreadLocalRandom
import cats.Show
import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.generator.NarrowTransactionGenerator.Settings
import com.ltonetwork.state.DataEntry.{MaxValueSize, Type}
import com.ltonetwork.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, IntegerDataEntry, StringDataEntry}
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.LoggerFacade
import org.slf4j.LoggerFactory

import scala.util.Random

class NarrowTransactionGenerator(settings: Settings, val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {

  private def r = ThreadLocalRandom.current

  private val log     = LoggerFacade(LoggerFactory.getLogger(getClass))
  private val typeGen = new DistributedRandomGenerator(settings.probabilities)

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  private def logOption[T <: Transaction](txE: Either[ValidationError, T])(implicit m: Manifest[T]): Option[T] = {
    txE match {
      case Left(e) =>
        log.warn(s"${m.runtimeClass.getName}: ${e.toString}")
        None
      case Right(tx) => Some(tx)
    }
  }

  override def next(): Iterator[Transaction] = generate(settings.transactions).toIterator

  def generate(n: Int): Seq[Transaction] = {
    val issueTransactionSender = randomFrom(accounts).get

    val tradeAssetDistribution = {
      accounts.map(acc => {
        TransferTransaction
          .selfSigned(1, System.currentTimeMillis(), issueTransactionSender, 5, acc, 100000, Array.fill(r.nextInt(100))(r.nextInt().toByte))
          .right
          .get
      })
    }

    val generated = (0 until (n * 1.2).toInt).foldLeft(
      (
        Seq.empty[Transaction],
        Seq.empty[LeaseTransaction]
      )) {
      case ((allTxsWithValid, activeLeaseTransactions), _) =>
        def moreThatStandardFee = 100000L + r.nextInt(100000)

        def ts = System.currentTimeMillis()

        val tx = typeGen.getRandom match {

          case TransferTransaction =>
            val recipient = randomFrom(accounts).get.toAddress
            val sender = randomFrom(accounts).get
            logOption(
              TransferTransaction.selfSigned(1, ts, sender, moreThatStandardFee, recipient, r.nextInt(500000), Array.fill(r.nextInt(100))(r.nextInt().toByte))
            )

          case LeaseTransaction =>
            val sender       = randomFrom(accounts).get
            val recipientOpt = randomFrom(accounts.filter(_ != sender).map(_.toAddress))
            recipientOpt.flatMap(recipient => logOption(LeaseTransaction.selfSigned(1, ts, sender, moreThatStandardFee * 3, recipient, 1)))
          case CancelLeaseTransaction =>
            randomFrom(activeLeaseTransactions).flatMap(lease => {
              val sender = accounts.find(_.address == lease.sender.address).get
              logOption(CancelLeaseTransaction.selfSigned(1, ts, sender, moreThatStandardFee * 3, lease.id()))
            })
          case MassTransferTransaction =>
            val transferCount = r.nextInt(MassTransferTransaction.MaxTransferCount)
            val transfers = for (i <- 0 to transferCount) yield {
              val recipient = randomFrom(accounts).get.toAddress
              val amount    = r.nextLong(500000)
              ParsedTransfer(recipient, amount)
            }
            val sender = randomFrom(accounts).get
            logOption(MassTransferTransaction.selfSigned(1,
                                                         ts,
                                                         sender,
                                                         100000 + 50000 * transferCount,
                                                         transfers.toList,
                                                         Array.fill(r.nextInt(100))(r.nextInt().toByte))
            )
          case DataTransaction =>
            val sender = randomFrom(accounts).get
            val count  = r.nextInt(10)

            val data = for {
              _ <- 0 until count
              keyLen = r.nextInt(10)
              key    = Random.nextString(keyLen)
              etype  = r.nextInt(Type.maxId)
            } yield
              etype match {
                case t if t == Type.Integer.id => IntegerDataEntry(key, r.nextLong)
                case t if t == Type.Boolean.id => BooleanDataEntry(key, r.nextBoolean)
                case t if t == Type.String.id  => StringDataEntry(key, r.nextLong.toString)
                case t if t == Type.Binary.id =>
                  val size = r.nextInt(MaxValueSize + 1)
                  val b    = new Array[Byte](size)
                  r.nextBytes(b)
                  BinaryDataEntry(key, ByteStr(b))
              }
            val size = 128 + data.map(_.toBytes.length).sum
            val fee  = 100000 * (size / 1024 + 1)
            logOption(DataTransaction.selfSigned(1, ts, sender, fee, data.toList))
        }

        (tx.map(tx => allTxsWithValid :+ tx).getOrElse(allTxsWithValid), tx match {
          case Some(tx: LeaseTransaction)     => activeLeaseTransactions :+ tx
          case Some(tx: CancelLeaseTransaction) => activeLeaseTransactions.filter(_.id() != tx.leaseId)
          case _                                => activeLeaseTransactions
        })
    }

    tradeAssetDistribution ++ generated._1.take(n - tradeAssetDistribution.size)
  }
}

object NarrowTransactionGenerator {

  case class Settings(transactions: Int, probabilities: Map[TransactionBuilder, Double])

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""transactions per iteration: $transactions
         |probabilities:
         |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

}
