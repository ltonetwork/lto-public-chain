package com.ltonetwork.http

import akka.http.scaladsl.model.StatusCodes
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.api.{InvalidAddress, InvalidSignature, TooBigArrayAllocation, TransactionsApiRoute}
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.fee.FeeCalculator
import com.ltonetwork.http.ApiMarshallers._
import com.ltonetwork.settings.{FeeSettings, FeesSettings, FunctionalitySettings, TestFunctionalitySettings, WalletSettings}
import com.ltonetwork.state.Blockchain
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.ltonetwork.utils.{Base58, _}
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import com.ltonetwork.{BlockGen, NoShrink, TestTime, TransactionGen, fee}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json._

class TransactionsRouteSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with MockFactory
    with Matchers
    with TransactionGen
    with BlockGen
    with ScalaCheckDrivenPropertyChecks
    with NoShrink {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  private val wallet      = Wallet(WalletSettings(None, "qwerty", None, None, None))
  private val featuresSettings = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures ++ Map(BlockchainFeatures.BurnFeeture.id -> 0)
  )
  private val blockchain  = mock[Blockchain]
  private val utx         = mock[UtxPool]
  private val allChannels = mock[ChannelGroup]
  private val allAccounts = wallet.privateKeyAccounts
  private val feesSettings = FeesSettings(
    Map[Byte, Seq[FeeSettings]](
      TransferTransaction.typeId     -> Seq(FeeSettings("BASE", 1.lto)),
      MassTransferTransaction.typeId -> Seq(FeeSettings("BASE", 1.lto), FeeSettings("VAR", 0.1.lto))
    ))
  private val feeCalculator = FeeCalculator(feesSettings, blockchain)
  private val route =
    TransactionsApiRoute(restAPISettings, featuresSettings, feeCalculator, wallet, blockchain, utx, allChannels, new TestTime).route

  def blockchainExpects(): Unit = {
    (blockchain.height _).expects().returning(1).anyNumberOfTimes()
    (blockchain.hasScript _).expects(*).returning(false).anyNumberOfTimes()
    (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures).anyNumberOfTimes()
    (blockchain.feePrice _).expects(1).returning(fee.defaultFeePrice).anyNumberOfTimes()
  }


  routePath("/calculateFee") - {
    "transfer with LTO fee" - {
      "TransferTransaction" in {
        val sender: PublicKeyAccount = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "senderPublicKey" -> Base58.encode(sender.publicKey),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        blockchainExpects()

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100 * 1000 * 1000L
        }
      }

      "MassTransferTransaction" in {
        val sender: PublicKeyAccount = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 11,
          "version"         -> 1,
          "senderPublicKey" -> Base58.encode(sender.publicKey),
          "transfers" -> Json.arr(
            Json.obj(
              "recipient" -> accountGen.sample.get.toAddress,
              "amount"    -> 1000000
            ),
            Json.obj(
              "recipient" -> accountGen.sample.get.toAddress,
              "amount"    -> 2000000
            )
          )
        )

        blockchainExpects()

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual (100 * 1000 * 1000L + 2 * 10 * 1000 * 1000L)
        }
      }
    }

  }

  routePath("/address/{address}") - {
    "handles invalid address" in {
      forAll(bytes32gen, choose(1, MaxTransactionsPerRequest)) {
        case (bytes, limit) =>
          Get(routePath(s"/address/${Base58.encode(bytes)}?limit=$limit")) ~> route should produce(InvalidAddress)
      }
    }

    "handles invalid limit" in {
      forAll(accountGen) {
        account =>
          Get(routePath(s"/address/${account.address}?limit=-1")) ~> route ~> check {
            status shouldEqual StatusCodes.BadRequest
            (responseAs[JsObject] \ "message").as[String] shouldEqual "invalid.limit"
          }
      }

      forAll(accountGen, choose(MaxTransactionsPerRequest + 1, Int.MaxValue).label("limitExceeded")) {
        case (account, limit) =>
          Get(routePath(s"/address/${account.address}?limit=$limit")) ~> route should produce(TooBigArrayAllocation)
      }
    }

  }

  routePath("/info/{signature}") - {
    "handles invalid signature" in {
      forAll(alphaNumStr.map(_ + "O")) { invalidBase58 =>
        Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx     <- randomTransactionGen
        height <- posNum[Int]
      } yield (tx, height)

      forAll(txAvailability) {
        case (tx, height) =>
          blockchainExpects()
          (blockchain.transactionInfo _).expects(tx.id()).returning(Some((height, tx))).once()

          Get(routePath(s"/info/${tx.id().base58}")) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            responseAs[JsValue] shouldEqual tx.json() ++ Json.obj(
              "height" -> JsNumber(height),
              "effectiveFee" -> tx.fee
            )
          }
      }
    }
  }

  routePath("/sign") - {
    "sign transaction" - {
      "TransferTransaction" in {
        val sender: PrivateKeyAccount = allAccounts.head
        val transferTx = Json.obj(
          "type"      -> 4,
          "version"   -> 2,
          "amount"    -> 1000000,
          "fee"       -> 100 * 1000 * 1000L,
          "sender"    -> sender.address,
          "recipient" -> accountGen.sample.get.toAddress
        )

        blockchainExpects()

        Post(routePath("/sign"), transferTx) ~> ApiKey(apiKey) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "timestamp").as[Long] should not be 0
          (responseAs[JsObject] \ "proofs").as[Proofs].proofs should not be empty
        }
      }
    }
  }

  routePath("/unconfirmed") - {
    "returns the list of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.all _).expects().returns(txs).once()
        Get(routePath("/unconfirmed")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val resp = responseAs[Seq[JsValue]]
          for ((r, t) <- resp.zip(txs)) {
            if ((r \ "signature").isDefined) (r \ "signature").as[String] shouldEqual t.proofs.toSignature.base58
            else (r \ "proofs").as[List[String]].head shouldEqual t.proofs.toSignature.toString
          }
        }
      }
    }
  }

  routePath("/unconfirmed/size") - {
    "returns the size of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.size _).expects().returns(txs.size).once()
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
      }
    }
  }

  routePath("/unconfirmed/info/{signature}") - {
    "handles invalid signature" in {
      forAll(alphaNumStr.map(_ + "O")) { invalidBase58 =>
        Get(routePath(s"/unconfirmed/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/unconfirmed/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/unconfirmed/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      forAll(randomTransactionGen) { tx =>
        (utx.transactionById _).expects(tx.id()).returns(Some(tx)).once()
        Get(routePath(s"/unconfirmed/info/${tx.id().base58}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual tx.json()
        }
      }
    }
  }
}
