package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.api.http.requests.sponsorship.SignedSponsorshipV1Request
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction, SponsorshipTransactionBase}
import com.ltonetwork.utils.Base58
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
    forAll(cancelSponsorshipGen)(tx => checkSerialization(tx, CancelSponsorshipTransaction.parseBytes))
  }

  property("serialization from TypedTransaction") {
    forAll(sponsorshipGen) { tx: SponsorshipTransactionBase =>
      val recovered = SponsorshipTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }

    forAll(cancelSponsorshipGen) { tx: SponsorshipTransactionBase =>
      val recovered = CancelSponsorshipTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedSponsorshipV1Request] = Json.format[SignedSponsorshipV1Request]

    forAll(sponsorshipGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedSponsorshipV1Request]
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
        chainId = None,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        recipient = p,
        fee = 100000,
        timestamp = 1526911531530L,
        sponsor = None,
        proofs = Proofs(Seq(arr))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
