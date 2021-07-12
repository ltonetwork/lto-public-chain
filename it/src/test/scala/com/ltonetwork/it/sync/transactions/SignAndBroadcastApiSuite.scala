package com.ltonetwork.it.sync.transactions

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.TransferRequest
import com.ltonetwork.crypto
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.sync._
import com.ltonetwork.it.transactions.BaseTransactionSuite
import com.ltonetwork.it.util._
import com.ltonetwork.state._
import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import com.ltonetwork.utils.Base58
import org.asynchttpclient.util.HttpConstants
import play.api.libs.json._

class SignAndBroadcastApiSuite extends BaseTransactionSuite {
  test("height should always be reported for transactions") {
    val txId = sender.transfer(firstAddress, secondAddress, 1.lto, fee = 1.lto).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    val jsv1               = Json.parse(sender.get(s"/transactions/info/$txId").getResponseBody)
    val hasPositiveHeight1 = (jsv1 \ "height").asOpt[Int].map(_ > 0)
    assert(hasPositiveHeight1.getOrElse(false))

    val response           = sender.get(s"/transactions/address/$firstAddress/limit/1")
    val jsv2               = Json.parse(response.getResponseBody).as[JsArray]
    val hasPositiveHeight2 = (jsv2(0)(0) \ "height").asOpt[Int].map(_ > 0)

    assert(hasPositiveHeight2.getOrElse(false))
  }

  ignore("/transactions/sign should handle erroneous input") {
    def assertSignBadJson(json: JsObject, expectedMessage: String) =
      assertBadRequestAndMessage(sender.postJsonWithApiKey("/transactions/sign", json), expectedMessage)

    for (v <- supportedVersions) {
      val json = Json.obj("type" -> 10, "sender" -> firstAddress, "alias" -> "alias", "fee" -> 100000)
      val js   = if (Option(v).isDefined) json ++ Json.obj("version" -> v.toInt) else json
      assertSignBadJson(js - "type", "failed to parse json message")
      assertSignBadJson(js + ("type" -> Json.toJson(-100)), "Bad transaction type")
      assertSignBadJson(js - "alias", "failed to parse json message")
    }
  }

  ignore("/transactions/sign should respect timestamp if specified") {
    val timestamp = 1500000000000L
    for (v <- supportedVersions) {
      val json = Json.obj("type" -> 10, "sender" -> firstAddress, "fee" -> 100000, "timestamp" -> timestamp)
      val js   = if (Option(v).isDefined) json ++ Json.obj("version" -> v.toInt) else json
      val r    = sender.postJsonWithApiKey("/transactions/sign", js)
      assert(r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      assert((Json.parse(r.getResponseBody) \ "timestamp").as[Long] == timestamp)
    }
  }

  ignore("/transactions/broadcast should handle erroneous input") {
    def assertBroadcastBadJson(json: JsObject, expectedMessage: String) =
      assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", json), expectedMessage)

    val timestamp = System.currentTimeMillis
    val jsonV1 = Json.obj(
      "type"            -> 10,
      "senderPublicKey" -> "8LbAU5BSrGkpk5wbjLMNjrbc9VzN9KBBYv9X8wGpmAJT",
      "fee"             -> 100000,
      "timestamp"       -> timestamp,
      "signature"       -> "A" * 64
    )

    assertBroadcastBadJson(jsonV1, "invalid signature")

    val jsonV2 = Json.obj(
      "type"            -> 10,
      "version"         -> 2,
      "senderPublicKey" -> "8LbAU5BSrGkpk5wbjLMNjrbc9VzN9KBBYv9X8wGpmAJT",
      "fee"             -> 100000,
      "timestamp"       -> timestamp,
      "proofs"          -> List("A" * 64)
    )

    assertBroadcastBadJson(jsonV2, "Script doesn't exist and proof doesn't validate")

    for (j <- List(jsonV1, jsonV2)) {
      assertBroadcastBadJson(j - "type", "failed to parse json message")
      assertBroadcastBadJson(j - "type" + ("type" -> Json.toJson(88)), "Bad transaction type")
      assertBroadcastBadJson(j - "alias", "failed to parse json message")
    }
  }

  test("/transactions/sign should produce transfer transaction that is good for /transactions/broadcast") {
    for (v <- supportedVersions) {
      signBroadcastAndCalcFee(
        Json.obj("type"       -> 4,
                 "sender"     -> firstAddress,
                 "recipient"  -> secondAddress,
                 "amount"     -> 1.lto,
                 "attachment" -> Base58.encode("falafel".getBytes)),
        usesProofs = Option(v).nonEmpty,
        version = Some(v)
      )
    }
  }

  test("/transactions/sign should produce mass transfer transaction that is good for /transactions/broadcast") {
    signBroadcastAndCalcFee(
      Json.obj(
        "type"       -> 11,
        "version"    -> 1,
        "sender"     -> firstAddress,
        "transfers"  -> Json.toJson(Seq(Transfer(secondAddress, 1.lto), Transfer(thirdAddress, 2.lto))),
        "attachment" -> Base58.encode("masspay".getBytes)
      ),
      usesProofs = true,
      fee = Some(120000000L)
    )
  }

  test("/transactions/sign should produce lease/cancel transactions that are good for /transactions/broadcast") {
    for (v <- supportedVersions) {
      val isProof = Option(v).nonEmpty
      val leaseId =
        signBroadcastAndCalcFee(Json.obj("type" -> 8, "sender" -> firstAddress, "amount" -> 1.lto, "recipient" -> secondAddress),
                                usesProofs = isProof,
                                version = Some(v))

      signBroadcastAndCalcFee(Json.obj("type" -> 9, "sender" -> firstAddress, "txId" -> leaseId), usesProofs = isProof, version = Some(v))
    }
  }

  ignore("/transactions/sign should produce data transaction that is good for /transactions/broadcast") {
    signBroadcastAndCalcFee(
      Json.obj(
        "type"    -> 12,
        "version" -> 1,
        "sender"  -> firstAddress,
        "data" -> List(
          IntegerDataEntry("int", 923275292849183L),
          BooleanDataEntry("bool", value = true),
          BinaryDataEntry("blob", ByteStr(Array.tabulate(445)(_.toByte))),
          StringDataEntry("str", "AAA-AAA")
        )
      ),
      usesProofs = true
    )
  }

  test("/transactions/sign should produce script transaction that is good for /transactions/broadcast") {
    signBroadcastAndCalcFee(
      Json.obj(
        "type"    -> 13,
        "version" -> 1,
        "sender"  -> firstAddress,
        "script"  -> None
      ),
      usesProofs = true
    )
  }

  test("/transactions/sign/{signerAddress} should sign a transaction by key of signerAddress") {
    val json = Json.obj(
      "type"      -> 4,
      "sender"    -> firstAddress,
      "recipient" -> secondAddress,
      "fee"       -> 25000000,
      "amount"    -> 1.lto
    )

    val signedRequestResponse = sender.postJsonWithApiKey(s"/transactions/sign/$thirdAddress", json)
    assert(signedRequestResponse.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
    val signedRequestJson = Json.parse(signedRequestResponse.getResponseBody)
    val signedRequest     = signedRequestJson.as[TransferRequest]
    assert(PublicKeyAccount.fromBase58String(signedRequest.senderPublicKey.get).explicitGet().address == firstAddress)
    assert(signedRequest.recipient == secondAddress)
    assert(signedRequest.fee == 25000000)
    assert(signedRequest.amount == 1.lto)
    val signature  = Base58.decode((signedRequestJson \ "signature").as[String]).get
    val tx         = signedRequest.toTx.explicitGet()
    val privateKey = pkByAddress(thirdAddress)
    assert(crypto.verify(signature, tx.bodyBytes(), privateKey.publicKey))
  }

  private def signBroadcastAndCalcFee(json: JsObject, usesProofs: Boolean, version: Option[String] = None, fee: Option[Long] = None): String = {
    val jsWithPK        = json ++ Json.obj("senderPublicKey" -> sender.publicKey.toString)
    val actualFee: Long = fee.getOrElse(sender.calculateFee(jsWithPK).feeAmount)
    val jsWithFee       = jsWithPK ++ Json.obj("fee" -> actualFee)
    val js              = jsWithFee ++ version.map(v => Json.obj("version" -> v.toInt)).getOrElse(Json.obj())
    val rs              = sender.postJsonWithApiKey("/transactions/sign", js)
    assert(rs.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
    val body = Json.parse(rs.getResponseBody)
    val signed: Boolean = if (usesProofs) {
      val proofs = (body \ "proofs").as[Seq[String]]
      proofs.lengthCompare(1) == 0 && proofs.head.nonEmpty
    } else (body \ "signature").as[String].nonEmpty
    assert(signed)
    val rb = sender.postJson("/transactions/broadcast", body)
    assert(rb.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
    val id = (Json.parse(rb.getResponseBody) \ "id").as[String]
    assert(id.nonEmpty)
    nodes.waitForHeightAriseAndTxPresent(id)
    id
  }
}
