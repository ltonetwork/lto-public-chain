package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.anchor.SignedAnchorV1Request
import com.ltonetwork.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, EitherExt2, IntegerDataEntry}
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.utils.Base58
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}
import scorex.crypto.encode.Base64

class AnchorTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: AnchorTransaction): Assertion = {
    val parsed = AnchorTransaction.parseBytes(tx.bytes()).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee

    parsed.anchors.zip(tx.anchors).foreach {
      case (r, t) =>
        r shouldEqual t
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(anchorTransactionGen)(checkSerialization)
  }

  property("serialization from AnchorTransaction") {
    forAll(anchorTransactionGen) { tx: AnchorTransaction =>
      val recovered = AnchorTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedAnchorV1Request] = Json.format[SignedAnchorV1Request]

    forAll(anchorTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedAnchorV1Request]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp shouldEqual tx.timestamp

      req.anchors zip tx.anchors foreach {
        case (re, te) =>
          re shouldEqual te.base58
      }
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 15,
                       "id": "9sFJpeo6y3txeNxrueWVgrEDU9zGAvk2CPh8N9Dd4fRM",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 1,
                       "anchors": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val arr = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get
    val tx = AnchorTransaction
      .create(
        1,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        List(arr),
        None,
        Proofs(Seq(arr))
      )
      .right
      .get

    tx.json() shouldEqual js
  }

}
