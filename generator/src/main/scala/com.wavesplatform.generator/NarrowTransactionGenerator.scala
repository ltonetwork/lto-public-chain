package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.generator.NarrowTransactionGenerator.Settings
import com.wavesplatform.state.DataEntry.{MaxValueSize, Type}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseCancelTransactionV1, LeaseTransactionV1}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.LoggerFacade
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
        TransferTransactionV1
          .selfSigned(issueTransactionSender, acc, 5, System.currentTimeMillis(), 100000, Array.fill(r.nextInt(100))(r.nextInt().toByte))
          .right
          .get
      })
    }

    val generated = (0 until (n * 1.2).toInt).foldLeft(
      (
        Seq.empty[Transaction],
        Seq.empty[LeaseTransactionV1]
      )) {
      case ((allTxsWithValid, activeLeaseTransactions), _) =>
        def moreThatStandartFee = 100000L + r.nextInt(100000)

        def ts = System.currentTimeMillis()

        val tx = typeGen.getRandom match {

          case TransferTransactionV1 =>
            val useAlias          = r.nextBoolean()
            val recipient         = randomFrom(accounts).get.toAddress
            val sendAsset         = r.nextBoolean()
            val senderAndAssetOpt = Some((randomFrom(accounts).get, None))
            senderAndAssetOpt.flatMap {
              case (sender, asset) =>
                logOption(
                  TransferTransactionV1
                    .selfSigned(sender, recipient, r.nextInt(500000), ts, moreThatStandartFee, Array.fill(r.nextInt(100))(r.nextInt().toByte)))
            }

          case LeaseTransactionV1 =>
            val sender       = randomFrom(accounts).get
            val useAlias     = r.nextBoolean()
            val recipientOpt = randomFrom(accounts.filter(_ != sender).map(_.toAddress))
            recipientOpt.flatMap(recipient => logOption(LeaseTransactionV1.selfSigned(sender, 1, moreThatStandartFee * 3, ts, recipient)))
          case LeaseCancelTransactionV1 =>
            randomFrom(activeLeaseTransactions).flatMap(lease => {
              val sender = accounts.find(_.address == lease.sender.address).get
              logOption(LeaseCancelTransactionV1.selfSigned(sender, lease.id(), moreThatStandartFee * 3, ts))
            })
          case MassTransferTransaction =>
            val transferCount = r.nextInt(MassTransferTransaction.MaxTransferCount)
            val transfers = for (i <- 0 to transferCount) yield {
              val useAlias  = r.nextBoolean()
              val recipient = randomFrom(accounts).get.toAddress
              val amount    = r.nextLong(500000)
              ParsedTransfer(recipient, amount)
            }
            val sendAsset         = r.nextBoolean()
            val senderAndAssetOpt = Some((randomFrom(accounts).get, None))
            senderAndAssetOpt.flatMap {
              case (sender, asset) =>
                logOption(
                  MassTransferTransaction.selfSigned(MassTransferTransaction.version,
                                                     sender,
                                                     transfers.toList,
                                                     ts,
                                                     100000 + 50000 * transferCount,
                                                     Array.fill(r.nextInt(100))(r.nextInt().toByte)))
            }
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
            logOption(DataTransaction.selfSigned(1, sender, data.toList, fee, ts))
        }

        (tx.map(tx => allTxsWithValid :+ tx).getOrElse(allTxsWithValid), tx match {
          case Some(tx: LeaseTransactionV1)     => activeLeaseTransactions :+ tx
          case Some(tx: LeaseCancelTransaction) => activeLeaseTransactions.filter(_.id != tx.leaseId)
          case _                                => activeLeaseTransactions
        })
    }

    tradeAssetDistribution ++ generated._1.take(n - tradeAssetDistribution.size)
  }
}

object NarrowTransactionGenerator {

  case class Settings(transactions: Int, probabilities: Map[TransactionParser, Double])

  private val minAliasLength = 4
  private val maxAliasLength = 30
  private val aliasAlphabet  = "-.0123456789@_abcdefghijklmnopqrstuvwxyz".toVector

  def generateAlias(): String = {
    val len = Random.nextInt(maxAliasLength - minAliasLength) + minAliasLength
    Random.shuffle(aliasAlphabet).take(len).mkString
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""transactions per iteration: $transactions
         |probabilities:
         |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

}
