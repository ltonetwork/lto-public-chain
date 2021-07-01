package com.ltonetwork.it

import java.util.concurrent.ThreadLocalRandom
import com.typesafe.config.Config
import com.ltonetwork.it.TransferSending.Req
import com.ltonetwork.it.api.AsyncHttpApi._
import com.ltonetwork.it.api.Transaction
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.utils.{Base58, ScorexLogging}
import org.scalatest.Suite
import com.ltonetwork.account.{Address, AddressScheme, PrivateKeyAccount}
import com.ltonetwork.api.http.requests
import com.ltonetwork.api.http.requests.TransferRequest
import com.ltonetwork.transaction.transfer._

import scala.concurrent.Future
import scala.util.Random

object TransferSending {
  case class Req(senderSeed: String, targetAddress: String, amount: Long, fee: Long)
}

trait TransferSending extends ScorexLogging {
  this: Suite with Nodes =>

  import scala.concurrent.ExecutionContext.Implicits.global

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'I'.toByte
  }

  def generateTransfersFromAccount(n: Int, accountAddress: String): Seq[Req] = {
    val fee      = 100000000 // + 400000 for scripted accounts
    val seedSize = 32

    val srcSeed = NodeConfigs.Default
      .collectFirst {
        case x if x.getString("address") == accountAddress => x.getString("account-seed")
      }
      .getOrElse(throw new RuntimeException(s"Can't find address '$accountAddress' in nodes.conf"))

    val sourceAndDest = (1 to n).map { _ =>
      val destPk = Array.fill[Byte](seedSize)(Random.nextInt(Byte.MaxValue).toByte)
      Address.fromPublicKey(destPk).address
    }

    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, dstAddr) =>
        rs :+ Req(srcSeed, dstAddr, fee, fee)
    }

    requests
  }

  def generateTransfersBetweenAccounts(n: Int, balances: Map[Config, Long]): Seq[Req] = {
    val fee = 100000000
    val srcDest = balances.toSeq
      .map {
        case (config, _) =>
          val accountSeed = config.getString("account-seed")
          (config, PrivateKeyAccount.fromSeed(accountSeed).explicitGet())
      }

    val sourceAndDest = (1 to n).map { _ =>
      val Seq((srcConfig, _), (_, destPrivateKey)) = Random.shuffle(srcDest).take(2)
      (srcConfig, destPrivateKey.address)
    }

    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (srcConfig, destAddr)) =>
        val a              = Random.nextDouble()
        val b              = balances(srcConfig)
        val transferAmount = (1e-8 + a * 1e-9 * b).toLong
        if (transferAmount < 0) log.warn(s"Negative amount: (1e-8 + $a * 1e-8 * $b) = $transferAmount")
        rs :+ Req(srcConfig.getString("account-seed"), destAddr, Math.max(transferAmount, 1L), fee)
    }

    requests
  }

  def generateTransfersToRandomAddresses(n: Int, excludeSrcAddresses: Set[String]): Seq[Req] = {
    val fee      = 100000000
    val seedSize = 32

    val seeds = NodeConfigs.Default.collect {
      case config if !excludeSrcAddresses.contains(config.getString("address")) => config.getString("account-seed")
    }

    val sourceAndDest = (1 to n).map { _ =>
      val srcSeed  = Random.shuffle(seeds).head
      val destPk   = Array.fill[Byte](seedSize)(Random.nextInt(Byte.MaxValue).toByte)
      val destAddr = Address.fromPublicKey(destPk).address

      (srcSeed, destAddr)
    }
    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (srcSeed, dstAddr)) =>
        rs :+ Req(srcSeed, dstAddr, fee, fee)
    }

    requests
  }

  def balanceForNode(n: Node): Future[(String, Long)] = n.balance(n.address).map(b => n.address -> b.balance)

  def processRequests(requests: Seq[Req], includeAttachment: Boolean = false): Future[Seq[Transaction]] = {
    val n     = requests.size
    val start = System.currentTimeMillis() - n
    val requestGroups = requests.zipWithIndex
      .map {
        case (x, i) =>
          createSignedTransferRequest(
            TransferTransaction
              .selfSigned(
                sender = PrivateKeyAccount.fromSeed(x.senderSeed).explicitGet(),
                recipient = Address.fromString(x.targetAddress).explicitGet(),
                amount = x.amount,
                timestamp = start + i,
                fee = x.fee,
                attachment = if (includeAttachment) {
                  Array.fill(TransferTransaction.MaxAttachmentSize)(ThreadLocalRandom.current().nextInt().toByte)
                } else Array.emptyByteArray,
                version = 2
              )
              .right
              .get)
      }
      .grouped(requests.size / nodes.size)
      .toSeq

    Future
      .traverse(nodes.zip(requestGroups)) { case (node, request) => node.batchSignedTransfer(request) }
      .map(_.flatten)
  }

  protected def createSignedTransferRequest(tx: TransferTransaction): TransferRequest = {
    import tx._
    requests.TransferRequest(
      version = Some(2),
      timestamp = Some(timestamp),
      sender = Some(Base58.encode(sender.publicKey)),
      fee = fee,
      recipient = recipient.stringRepr,
      amount = amount,
      attachment = Some(ByteStr(attachment)),
      proofs = Some(proofs)
    )
  }

}
