package com.ltonetwork.it.api

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCodes.BadRequest
import com.ltonetwork.api.http.AddressApiRoute
import com.ltonetwork.api.http.SponsorshipApiRoute.SponsorshipInfo
import com.ltonetwork.features.api.ActivationStatus
import com.ltonetwork.http.DebugMessage
import com.ltonetwork.it.Node
import com.ltonetwork.state.DataEntry
import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import org.asynchttpclient.Response
import org.scalactic.source.Position
import org.scalatest.{Assertion, Assertions, Matchers}
import play.api.libs.json.Json.parse
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Try}

object SyncHttpApi extends Assertions {
  case class ErrorMessage(error: Int, message: String)
  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

  case class NotFoundErrorMessage(status: String, details: String)

  object NotFoundErrorMessage {
    implicit val format: Format[NotFoundErrorMessage] = Json.format
  }

  def assertBadRequest[R](f: => R, expectedStatusCode: Int = 400): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, _)) => Assertions.assert(statusCode == expectedStatusCode)
    case Failure(e)                                               => Assertions.fail(e)
    case _                                                        => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndResponse[R](f: => R, errorRegex: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      lazy val assert = responseBody.replace("\n", "").matches(s".*$errorRegex.*")
      Assertions.assert(statusCode == BadRequest.intValue &&
                          responseBody.replace("\n", "").matches(s".*$errorRegex.*"),
                        s"expected '$errorRegex' but got '$responseBody")
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndMessage[R](f: => R, errorMessage: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      Assertions.assert(statusCode == BadRequest.intValue && parse(responseBody).as[ErrorMessage].message.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail(s"Expecting bad request")
  }

  def assertNotFoundAndMessage[R](f: => R, errorMessage: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      Assertions.assert(statusCode == StatusCodes.NotFound.intValue && parse(responseBody).as[NotFoundErrorMessage].details.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail(s"Expecting not found error")
  }

  implicit class NodeExtSync(n: Node) extends Assertions with Matchers {

    import com.ltonetwork.it.api.AsyncHttpApi.{NodeAsyncHttpApi => async}

    private val RequestAwaitTime = 15.seconds

    def get(path: String): Response =
      Await.result(async(n).get(path), RequestAwaitTime)

    def utx = Await.result(async(n).utx, RequestAwaitTime)

    def utxSize = Await.result(async(n).utxSize, RequestAwaitTime)

    def printDebugMessage(db: DebugMessage): Response =
      Await.result(async(n).printDebugMessage(db), RequestAwaitTime)

    def activationStatus: ActivationStatus =
      Await.result(async(n).activationStatus, RequestAwaitTime)

    def seed(address: String): String =
      Await.result(async(n).seed(address), RequestAwaitTime)

    def postJson[A: Writes](path: String, body: A): Response =
      Await.result(async(n).postJson(path, body), RequestAwaitTime)

    def postJsonWithApiKey[A: Writes](path: String, body: A): Response =
      Await.result(async(n).postJsonWithApiKey(path, body), RequestAwaitTime)

    def accountBalances(acc: String): (Long, Long) =
      Await.result(async(n).accountBalances(acc), RequestAwaitTime)

    def assertBalances(acc: String, balance: Long)(implicit pos: Position): Unit =
      Await.result(async(n).assertBalances(acc, balance, effectiveBalance = balance), RequestAwaitTime)

    def assertBalances(acc: String, balance: Long, effectiveBalance: Long)(implicit pos: Position): Unit =
      Await.result(async(n).assertBalances(acc, balance, effectiveBalance), RequestAwaitTime)

    def assertAssetBalance(acc: String, assetIdString: String, balance: Long)(implicit pos: Position): Unit =
      Await.result(async(n).assertAssetBalance(acc, assetIdString, balance), RequestAwaitTime)

    def assetBalance(address: String, asset: String): AssetBalance =
      Await.result(async(n).assetBalance(address, asset), RequestAwaitTime)

    def assetsDetails(assetId: String): AssetInfo =
      Await.result(async(n).assetsDetails(assetId), RequestAwaitTime)

    def addressScriptInfo(address: String): AddressApiRoute.AddressScriptInfo =
      Await.result(async(n).scriptInfo(address), RequestAwaitTime)

    def assetsBalance(address: String): FullAssetsInfo =
      Await.result(async(n).assetsBalance(address), RequestAwaitTime)

    def transactionInfo(txId: String): TransactionInfo =
      Await.result(async(n).transactionInfo(txId), RequestAwaitTime)

    def transactionsByAddress(address: String, limit: Int): Seq[Seq[TransactionInfo]] =
      Await.result(async(n).transactionsByAddress(address, limit), RequestAwaitTime)

    def scriptCompile(code: String): CompiledScript =
      Await.result(async(n).scriptCompile(code), RequestAwaitTime)

    def getAddresses: Seq[String] = Await.result(async(n).getAddresses, RequestAwaitTime)

    def sign(jsObject: JsObject): JsObject =
      Await.result(async(n).sign(jsObject), RequestAwaitTime)

    def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Transaction =
      Await.result(async(n).transfer(sourceAddress, recipient, amount, fee), RequestAwaitTime)

    def massTransfer(sourceAddress: String, transfers: List[Transfer], fee: Long, assetId: Option[String] = None): Transaction =
      Await.result(async(n).massTransfer(sourceAddress, transfers, fee, assetId), RequestAwaitTime)

    def lease(sourceAddress: String, recipient: String, leasingAmount: Long, leasingFee: Long): Transaction =
      Await.result(async(n).lease(sourceAddress, recipient, leasingAmount, leasingFee), RequestAwaitTime)

    def putData(sourceAddress: String, data: List[DataEntry[_]], fee: Long): Transaction =
      Await.result(async(n).putData(sourceAddress, data, fee), RequestAwaitTime)

    def getData(sourceAddress: String): List[DataEntry[_]] =
      Await.result(async(n).getData(sourceAddress), RequestAwaitTime)

    def getData(sourceAddress: String, key: String): DataEntry[_] =
      Await.result(async(n).getData(sourceAddress, key), RequestAwaitTime)

    import com.ltonetwork.api.http.AssociationsApiRoute.AssociationsInfo

    def getAssociations(address: String): AssociationsInfo =
      Await.result(async(n).getAssociations(address), RequestAwaitTime)

    def getSponsorship(address: String): SponsorshipInfo =
      Await.result(async(n).getSponsorship(address), RequestAwaitTime)

    def broadcastRequest[A: Writes](req: A): Transaction =
      Await.result(async(n).broadcastRequest(req), RequestAwaitTime)

    def activeLeases(sourceAddress: String): Seq[Transaction] =
      Await.result(async(n).activeLeases(sourceAddress), RequestAwaitTime)

    def cancelLease(sourceAddress: String, leaseId: String, fee: Long): Transaction =
      Await.result(async(n).cancelLease(sourceAddress, leaseId, fee), RequestAwaitTime)

    def signedBroadcast(tx: JsObject): Transaction =
      Await.result(async(n).signedBroadcast(tx), RequestAwaitTime)

    def ensureTxDoesntExist(txId: String): Unit =
      Await.result(async(n).ensureTxDoesntExist(txId), RequestAwaitTime)

    def createAddress(): String =
      Await.result(async(n).createAddress, RequestAwaitTime)

    def rawTransactionInfo(txId: String): JsValue =
      Await.result(async(n).rawTransactionInfo(txId), RequestAwaitTime)

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): TransactionInfo =
      Await.result(async(n).waitForTransaction(txId), RequestAwaitTime)

    def signAndBroadcast(tx: JsObject): Transaction =
      Await.result(async(n).signAndBroadcast(tx), RequestAwaitTime)

    def waitForHeight(expectedHeight: Int, requestAwaitTime: FiniteDuration = RequestAwaitTime): Int =
      Await.result(async(n).waitForHeight(expectedHeight), requestAwaitTime)

    def debugMinerInfo(): Seq[State] =
      Await.result(async(n).debugMinerInfo(), RequestAwaitTime)

    def height: Int =
      Await.result(async(n).height, RequestAwaitTime)

    def blockAt(height: Int): Block = Await.result(async(n).blockAt(height), RequestAwaitTime)

    def blockHeadersSeq(from: Int, to: Int): Seq[BlockHeaders] = Await.result(async(n).blockHeadersSeq(from, to), RequestAwaitTime)

    def rollback(to: Int, returnToUTX: Boolean = true): Unit =
      Await.result(async(n).rollback(to, returnToUTX), RequestAwaitTime)

    def findTransactionInfo(txId: String): Option[TransactionInfo] = Await.result(async(n).findTransactionInfo(txId), RequestAwaitTime)

    def connectedPeers: Seq[Peer] = (Json.parse(get("/peers/connected").getResponseBody) \ "peers").as[Seq[Peer]]

    def calculateFee(tx: JsObject): FeeInfo =
      Await.result(async(n).calculateFee(tx), RequestAwaitTime)
  }

  implicit class NodesExtSync(nodes: Seq[Node]) {

    import com.ltonetwork.it.api.AsyncHttpApi.{NodesAsyncHttpApi => async}

    private val TxInBlockchainAwaitTime = 20 * nodes.head.blockDelay
    private val ConditionAwaitTime      = 5.minutes

    def height(implicit pos: Position): Seq[Int] =
      Await.result(async(nodes).height, TxInBlockchainAwaitTime)

    def waitForHeightAriseAndTxPresent(transactionId: String)(implicit pos: Position): Unit =
      Await.result(async(nodes).waitForHeightAriseAndTxPresent(transactionId), TxInBlockchainAwaitTime)

    def waitForHeightArise(): Unit =
      Await.result(async(nodes).waitForHeightArise(), TxInBlockchainAwaitTime)

    def waitForSameBlockHeadesAt(height: Int, retryInterval: FiniteDuration = 5.seconds): Boolean =
      Await.result(async(nodes).waitForSameBlockHeadesAt(height, retryInterval), ConditionAwaitTime)

    def waitFor[A](desc: String)(retryInterval: FiniteDuration)(request: Node => A, cond: Iterable[A] => Boolean): Boolean =
      Await.result(
        async(nodes).waitFor(desc)(retryInterval)((n: Node) => Future(request(n))(scala.concurrent.ExecutionContext.Implicits.global), cond),
        ConditionAwaitTime)

    def rollback(height: Int, returnToUTX: Boolean = true): Unit = {
      Await.result(
        Future.traverse(nodes) { node =>
          com.ltonetwork.it.api.AsyncHttpApi.NodeAsyncHttpApi(node).rollback(height, returnToUTX)
        },
        ConditionAwaitTime
      )
    }

    def waitForHeight(height: Int): Unit = {
      Await.result(
        Future.traverse(nodes) { node =>
          com.ltonetwork.it.api.AsyncHttpApi.NodeAsyncHttpApi(node).waitForHeight(height)
        },
        ConditionAwaitTime
      )
    }
  }

}
