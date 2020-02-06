package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.wavesplatform.RequestGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Diff
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.utx.{UtxBatchOps, UtxPool}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen._
import org.scalacheck.{Gen => G}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.assets._
import com.wavesplatform.utils.Base58
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Proofs, Transaction, ValidationError}
import com.wavesplatform.wallet.Wallet
import shapeless.Coproduct

class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings    = RestAPISettings.fromConfig(ConfigFactory.load())
  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]

  (utx.putIfNew _).when(*).onCall((t: Transaction) => Left(TransactionValidationError(GenericError("foo"), t))).anyNumberOfTimes()

  "compatibility" - {
    val alwaysApproveUtx = stub[UtxPool]
    val utxOps = new UtxBatchOps {
      override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = alwaysApproveUtx.putIfNew(tx)
    }
    (alwaysApproveUtx.batched[Any] _).when(*).onCall((f: UtxBatchOps => Any) => f(utxOps)).anyNumberOfTimes()
    (alwaysApproveUtx.putIfNew _).when(*).onCall((_: Transaction) => Right((true, Diff.empty))).anyNumberOfTimes()

    val alwaysSendAllChannels = stub[ChannelGroup]
    (alwaysSendAllChannels.writeAndFlush(_: Any)).when(*).onCall((_: Any) => null).anyNumberOfTimes()

    val route = AssetsBroadcastApiRoute(settings, alwaysApproveUtx, alwaysSendAllChannels).route

    val seed               = "seed".getBytes()
    val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
    val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

    val transferRequest = createSignedTransferRequest(
      TransferTransactionV1
        .selfSigned(
          sender = senderPrivateKey,
          recipient = receiverPrivateKey.toAddress,
          amount = 1 * Waves,
          timestamp = System.currentTimeMillis(),
          feeAmount = Waves / 3,
          attachment = Array.emptyByteArray
        )
        .right
        .get
    )

    val versionedTransferRequest = createSignedVersionedTransferRequest(
      TransferTransactionV2
        .create(
          sender = senderPrivateKey,
          recipient = receiverPrivateKey.toAddress,
          amount = 1 * Waves,
          timestamp = System.currentTimeMillis(),
          feeAmount = Waves / 3,
          attachment = Array.emptyByteArray,
          version = 2,
          proofs = Proofs(Seq.empty)
        )
        .right
        .get)

    "/batch-transfer" - {
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("batch-transfer"), v).addHeader(ApiKeyHeader) ~> route

      "accepts TransferRequest" in posting(List(transferRequest)) ~> check {
        status shouldBe StatusCodes.OK
        val xs = responseAs[Seq[TransferTransactions]]
        xs.size shouldBe 1
        xs.head.select[TransferTransactionV1] shouldBe defined
      }

      "accepts VersionedTransferRequest" in posting(List(versionedTransferRequest)) ~> check {
        status shouldBe StatusCodes.OK
        val xs = responseAs[Seq[TransferTransactions]]
        xs.size shouldBe 1
        xs.head.select[TransferTransactionV2] shouldBe defined
      }

      "accepts both TransferRequest and VersionedTransferRequest" in {
        val reqs = List(
          Coproduct[SignedTransferRequests](transferRequest),
          Coproduct[SignedTransferRequests](versionedTransferRequest)
        )

        posting(reqs) ~> check {
          status shouldBe StatusCodes.OK
          val xs = responseAs[Seq[TransferTransactions]]
          xs.size shouldBe 2
          xs.flatMap(_.select[TransferTransactionV1]) shouldNot be(empty)
          xs.flatMap(_.select[TransferTransactionV2]) shouldNot be(empty)
        }
      }

    }

  }

  protected def createSignedTransferRequest(tx: TransferTransactionV1): SignedTransferV1Request = {
    import tx._
    SignedTransferV1Request(
      Base58.encode(tx.sender.publicKey),
      recipient.stringRepr,
      amount,
      fee,
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      signature.base58
    )
  }

  protected def createSignedVersionedTransferRequest(tx: TransferTransactionV2): SignedTransferV2Request = {
    import tx._
    SignedTransferV2Request(
      Base58.encode(tx.sender.publicKey),
      recipient.stringRepr,
      amount,
      fee,
      timestamp,
      version,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      proofs.proofs.map(_.base58).toList
    )
  }

}
