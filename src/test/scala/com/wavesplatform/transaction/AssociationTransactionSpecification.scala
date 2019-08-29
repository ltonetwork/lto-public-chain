package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.api.http.SignedAssociationRequest
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.utils.Base58
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}

class AssociationTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: AssociationTransaction): Assertion = {
    val parsed = AssociationTransaction.parseBytes(tx.bytes()).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.assoc shouldEqual tx.assoc

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(assocTransactionGen)(checkSerialization)
  }

  property("serialization from TypedTransaction") {
    forAll(assocTransactionGen) { tx: AssociationTransaction =>
      val recovered = AssociationTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedAssociationRequest] = Json.format[SignedAssociationRequest]

    forAll(assocTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedAssociationRequest]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp shouldEqual tx.timestamp
      req.associationType shouldEqual tx.assoc.assocType
      req.party shouldEqual tx.assoc.party.toString
      if (tx.assoc.hash.isDefined)
        req.hash shouldEqual tx.assoc.hash.map(_.base58)
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 16,
                       "id": "GAuCkFNM8CzCtoURGBVQn9FXkhvz8vXwq2KHZbDmCYwE",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 1,
                       "party" : "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "associationType" : 420,
                       "hash" : null
                       }
  """)

    val arr = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get
    val tx = AssociationTransaction
      .create(
        version = 1,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        party = Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").right.get,
        assocType = 420,
        hash = None,
        feeAmount = 100000,
        timestamp = 1526911531530L,
        proofs = Proofs(Seq(arr))
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
