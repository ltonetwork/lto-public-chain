package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.api.requests.ClaimRequest
import com.ltonetwork.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, DataEntry, EitherExt2, IntegerDataEntry}
import com.ltonetwork.transaction.claim.ClaimTransaction
import com.ltonetwork.utils.Base58
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json
import scorex.crypto.encode.Base64

class ClaimTransactionSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: ClaimTransaction): Assertion = {
    val tryParse = ClaimTransaction.parseBytes(tx.bytes())
    tryParse should be a 'success

    val parsed = tryParse.get
    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.sponsor shouldEqual tx.sponsor

    parsed.claimType shouldEqual tx.claimType
    parsed.subject shouldEqual tx.subject
    parsed.recipient shouldEqual tx.recipient

    parsed.data.zip(tx.data).foreach {
      case (r, t) =>
        r.key shouldEqual t.key
        r.value shouldEqual t.value
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(claimTransactionGen)(checkSerialization)
  }

  property("from TransactionBuilder") {
    forAll(claimTransactionGen) { tx: ClaimTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(claimTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[ClaimRequest]
      req.senderPublicKey should be ('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be ('defined)
      req.timestamp.get shouldEqual tx.timestamp
    }
  }

  property(testName = "JSON format validation (minimal)") {
    val js = Json.parse("""{
                       "type": 20,
                       "version": 3,
                       "id": "27ypi5gPqh5UJgn5oTRriR4qXQ89vNvXKikiq29Sd5y2",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "claimType": 42,
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val proof = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get

    val tx = ClaimTransaction
      .create(
        version = 3,
        chainId = None,
        timestamp = 1526911531530L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 100000,
        claimType = 42,
        recipient = None,
        subject = None,
        data = List.empty[DataEntry[_]],
        sponsor = None,
        proofs = Proofs(Seq(proof))
      ).explicitGet()

    tx.json() shouldEqual js
  }

  property(testName = "JSON format validation (full)") {
    val js = Json.parse("""{
                       "type": 20,
                       "version": 3,
                       "id": "6gPwhssDXRyLTm4F4PUugPrD4kJqFjxwXKJGhHDAgJYC",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "claimType": 42,
                       "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "subject": "dRsU",
                       "data": [
                         {
                           "key": "int",
                           "type": "integer",
                           "value": 24
                           },
                         {
                           "key": "bool",
                           "type": "boolean",
                           "value": true
                           },
                         {
                           "key": "blob",
                           "type": "binary",
                           "value": "base64:YWxpY2U="
                           }
                       ],
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94",
                         "2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx"
                       ]
                       }
  """)

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))

    val proofs = Seq(
      ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get,
      ByteStr.decodeBase58("2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx").get
    )

    val tx = ClaimTransaction
      .create(
        version = 3,
        chainId = None,
        timestamp = 1526911531530L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 100000,
        claimType = 42,
        recipient = Some(Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet()),
        subject = Some(ByteStr.decodeBase58("dRsU").get),
        data = List(entry1, entry2, entry3),
        sponsor = Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        proofs = Proofs(proofs)
      ).explicitGet()

    tx.json() shouldEqual js
  }

}
