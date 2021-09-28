package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.ClaimRequest
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.claim.ClaimTransaction
import com.ltonetwork.utils.Base58
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

class ClaimTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: ClaimTransaction): Assertion = {
    val tryParse = ClaimTransaction.parseBytes(tx.bytes())
    tryParse should be a 'success

    val parsed = tryParse.get
    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.sponsor shouldEqual tx.sponsor

    parsed.anchors.zip(tx.anchors).foreach {
      case (r, t) =>
        r shouldEqual t
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forEvery(versionTable(ClaimTransaction)) { version: Byte =>
      forAll(claimTransactionGen(version))(checkSerialization)
    }
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

      req.anchors zip tx.anchors foreach {
        case (re, te) =>
          re shouldEqual te.base58
      }
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 20,
                       "version": 3,
                       "id": "4kdm76fxCDCV3P9uXX5acfNvCg2AhMkVAXDdJimtapxo",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "anchors": [
                         "264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu"
                       ],
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val anchor = ByteStr.decodeBase58("264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu").get
    val proof = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get

    val tx = ClaimTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        List(anchor),
        None,
        Proofs(Seq(proof))
      ).explicitGet()

    tx.json() shouldEqual js
  }

  property(testName = "JSON format validation for sponsored tx") {
    val js = Json.parse("""{
                       "type": 20,
                       "version": 3,
                       "id": "BaSEvxZzFaujdA3Jt5kLKhnSy4w2vhcuHn9uuzyLKt4o",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "anchors": [
                         "264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu",
                         "31CS3LiQKFAo5sipHDc4vKCDJD4iQ5BkVwLEqjEaoBjZgQ6t6KbwPNpYxgEnbhtbbLmJcuKCxnJeP2zoDT1L1YYc"
                       ],
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94",
                         "2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx"
                       ]
                       }
  """)

    val anchors = List(
      ByteStr.decodeBase58("264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu").get,
      ByteStr.decodeBase58("31CS3LiQKFAo5sipHDc4vKCDJD4iQ5BkVwLEqjEaoBjZgQ6t6KbwPNpYxgEnbhtbbLmJcuKCxnJeP2zoDT1L1YYc").get
    )
    val proofs = Seq(
      ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get,
      ByteStr.decodeBase58("2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx").get
    )

    val tx = ClaimTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        anchors,
        Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        Proofs(proofs)
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
