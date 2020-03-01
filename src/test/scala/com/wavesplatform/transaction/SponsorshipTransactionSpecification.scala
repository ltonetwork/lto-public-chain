package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.api.http.SignedSponsorshipRequest
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.utils.Base58
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}

import scala.util.Try

class SponsorshipTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: SponsorshipTransactionBase, parser: Array[Byte] => Try[SponsorshipTransactionBase]): Assertion = {
    val parsed = parser(tx.bytes()).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.recipient shouldEqual tx.recipient

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(sponsorshipGen)(tx => checkSerialization(tx, SponsorshipTransaction.parseBytes))
    forAll(sponsorshipCancelGen)(tx => checkSerialization(tx, SponsorshipCancelTransaction.parseBytes))
  }

  property("serialization from TypedTransaction") {
    forAll(sponsorshipGen) { tx: SponsorshipTransactionBase =>
      val recovered = SponsorshipTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }

    forAll(sponsorshipCancelGen) { tx: SponsorshipTransactionBase =>
      val recovered = SponsorshipCancelTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedSponsorshipRequest] = Json.format[SignedSponsorshipRequest]

    forAll(sponsorshipGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedSponsorshipRequest]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp shouldEqual tx.timestamp
      req.recipient shouldEqual tx.recipient.stringRepr
    }
  }

  property(testName = "JSON format validation") {
    val p  = PrivateKeyAccount.fromSeed("xxx").explicitGet().toAddress
    val js = Json.parse(s"""{
                       "type": 18,
                       "id": "6Cet5iVQ6STk4HxcRL2p5A9vQKnaPwoJxZZyAAo6SwQa",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 1,
                       "recipient" : "$p"
                       }
  """)

    val arr = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get
    val tx = SponsorshipTransaction
      .create(
        version = 1,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        recipient = p,
        feeAmount = 100000,
        timestamp = 1526911531530L,
        proofs = Proofs(Seq(arr))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
