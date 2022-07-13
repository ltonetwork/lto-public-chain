package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.api.requests.{IssueAssociationRequest, RevokeAssociationRequest}
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.utils.Base58
import org.scalatest.{Assertion, OptionValues}
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

import scala.util.Try

class RevokeAssociationTransactionSpecification
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with OptionValues
    with TableDrivenPropertyChecks
    with Matchers
    with TransactionGen {

  private def checkSerialization(tx: RevokeAssociationTransaction, parser: Array[Byte] => Try[RevokeAssociationTransaction]): Assertion = {
    val bytes  = tx.bytes()
    val parsed = parser(bytes).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.recipient shouldEqual tx.recipient
    parsed.assocType shouldEqual tx.assocType
    parsed.subject shouldEqual tx.subject

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip versions") {
    forEvery(versionTable(RevokeAssociationTransaction)) { version: Byte =>
      forAll(revokeAssocTransactionGen(version))(tx => checkSerialization(tx, RevokeAssociationTransaction.parseBytes))
    }
  }

  property("serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(revokeAssocTransactionGen(3.toByte, keyType))(tx => checkSerialization(tx, RevokeAssociationTransaction.parseBytes))
    }
  }

  property("TransactionBuilder") {
    forAll(revokeAssocTransactionGen) { tx: RevokeAssociationTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(revokeAssocTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[RevokeAssociationRequest]
      req.senderPublicKey should be('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be('defined)
      req.timestamp.get shouldEqual tx.timestamp
      req.associationType shouldEqual tx.assocType
      req.recipient shouldEqual tx.recipient.toString
      req.subject shouldEqual tx.subject
    }
  }

  property(testName = "JSON format validation v1") {
    val js = Json.parse(s"""{
                       "type": 17,
                       "version": 1,
                       "id": "4VP33Gyxy5jYiWETBHTDQbCmnVFW4zj8GDg4JLHQFKBA",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "associationType" : 420,
                       "recipient" : "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val arr = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get
    val tx = RevokeAssociationTransaction
      .create(
        version = 1,
        chainId = None,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        assocType = 420,
        recipient = Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        subject = None,
        fee = 100000,
        timestamp = 1526911531530L,
        sponsor = None,
        proofs = Proofs(Seq(arr))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

  property(testName = "JSON format validation v4") {
    val js = Json.parse(s"""{
                       "type": 17,
                       "version": 4,
                       "id": "J4bSAgFJ19FVMSRSq3LqzLQjydJaUxXT5CDzxA828Jtf",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "associationType": 420,
                       "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "subject": "264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu",
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94",
                         "2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx"
                       ]
                       }
  """)

    val subject = ByteStr.decodeBase58("264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu").get
    val proofs = Seq(
      ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get,
      ByteStr.decodeBase58("2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx").get
    )
    val tx = RevokeAssociationTransaction
      .create(
        version = 4,
        chainId = None,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        assocType = 420,
        recipient = Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        subject = Some(subject),
        fee = 100000,
        timestamp = 1526911531530L,
        sponsor = Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        proofs = Proofs(proofs)
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
