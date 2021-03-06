package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.api.http.requests.association.SignedIssueAssociationV1Request
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.utils.Base58
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}

import scala.util.Try

class AssociationTransactionSpecification extends PropSpec with PropertyChecks with OptionValues with Matchers with TransactionGen {

  private def checkSerialization(tx: AssociationTransaction, parser: Array[Byte] => Try[AssociationTransaction]): Assertion = {
    val parsed = parser(tx.bytes()).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.assoc shouldEqual tx.assoc

    parsed.bytes() shouldEqual tx.bytes()
  }
  val issueGen  = assocTransactionGen suchThat (_.isInstanceOf[IssueAssociationTransaction]) map (_.asInstanceOf[IssueAssociationTransaction])
  val revokeGen = assocTransactionGen suchThat (_.isInstanceOf[RevokeAssociationTransaction]) map (_.asInstanceOf[RevokeAssociationTransaction])

  property("serialization roundtrip") {
    forAll(issueGen)(tx => checkSerialization(tx, IssueAssociationTransaction.parseBytes))
    forAll(revokeGen)(tx => checkSerialization(tx, RevokeAssociationTransaction.parseBytes))
  }

  property("serialization from TypedTransaction") {
    forAll(issueGen) { tx: AssociationTransaction =>
      val recovered = IssueAssociationTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }

    forAll(revokeGen) { tx: AssociationTransaction =>
      val recovered = RevokeAssociationTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedIssueAssociationV1Request] = Json.format[SignedIssueAssociationV1Request]

    forAll(assocTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedIssueAssociationV1Request]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp shouldEqual tx.timestamp
      req.associationType shouldEqual tx.assocType
      req.party shouldEqual tx.recipient.toString
      if (tx.hash.isDefined) req.hash.value shouldEqual tx.hash.get.base58
      else tx.hash shouldEqual None
    }
  }

  property(testName = "JSON format validation") {
    val p  = PrivateKeyAccount.fromSeed("xxx").explicitGet().toAddress
    val js = Json.parse(s"""{
                       "type": 16,
                       "version": 1,
                       "id": "GCRa1NZP34rkvRKxkJkisbvxPZX9sKrVLLqLmi8LvKjx",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "associationType" : 420,
                       "party" : "$p",
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val arr = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get
    val tx = IssueAssociationTransaction
      .create(
        version = 1,
        chainId = None,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        recipient = p,
        assocType = 420,
        hash = None,
        fee = 100000,
        timestamp = 1526911531530L,
        expires = None,
        sponsor = None,
        proofs = Proofs(Seq(arr))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
