package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.api.requests.IssueAssociationRequest
import com.ltonetwork.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, DataEntry, EitherExt2, IntegerDataEntry}
import com.ltonetwork.transaction.association.IssueAssociationTransaction
import com.ltonetwork.utils.Base58
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.Assertion
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import play.api.libs.json.Json
import scorex.crypto.encode.Base64

import scala.util.Try

class IssueAssociationTransactionSpecification
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with OptionValues
    with TableDrivenPropertyChecks
    with Matchers
    with TransactionGen {

  private def checkSerialization(tx: IssueAssociationTransaction, parser: Array[Byte] => Try[IssueAssociationTransaction]): Assertion = {
    val bytes  = tx.bytes()
    val parsed = parser(bytes).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.recipient shouldEqual tx.recipient
    parsed.assocType shouldEqual tx.assocType
    parsed.subject shouldEqual tx.subject

    parsed.expires shouldEqual tx.expires
    parsed.data shouldEqual tx.data

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip versions") {
    forEvery(versionTable(IssueAssociationTransaction)) { version: Byte =>
      forAll(issueAssocTransactionGen(version))(tx => checkSerialization(tx, IssueAssociationTransaction.parseBytes))
    }
  }

  property("serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(issueAssocTransactionGen(3.toByte, keyType))(tx => checkSerialization(tx, IssueAssociationTransaction.parseBytes))
    }
  }

  property("TransactionBuilder") {
    forAll(issueAssocTransactionGen) { tx: IssueAssociationTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(issueAssocTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[IssueAssociationRequest]
      req.senderPublicKey should be('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be('defined)
      req.timestamp.get shouldEqual tx.timestamp
      req.associationType shouldEqual tx.assocType
      req.recipient shouldEqual tx.recipient.toString
      req.subject shouldEqual tx.subject
      req.expires shouldEqual tx.expires
      req.data.getOrElse(List.empty[DataEntry[_]]) shouldEqual tx.data
    }
  }

  property(testName = "JSON format validation v1") {
    val p  = PrivateKeyAccount.fromSeed("xxx").explicitGet().toAddress
    val js = Json.parse(s"""{
                       "type": 16,
                       "version": 1,
                       "id": "GCRa1NZP34rkvRKxkJkisbvxPZX9sKrVLLqLmi8LvKjx",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "associationType" : 420,
                       "recipient" : "$p",
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
        subject = None,
        fee = 100000,
        timestamp = 1526911531530L,
        expires = None,
        data = List.empty,
        sponsor = None,
        proofs = Proofs(Seq(arr))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

  property(testName = "JSON format validation v4") {
    val js = Json.parse(s"""{
                       "type": 16,
                       "version": 4,
                       "id": "5aRZcGMcepDixKYYH8WNXbKwbkfTtsePtyWiW26qY2hK",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "associationType": 420,
                       "expires": 1558447531530,
                       "subject": "264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu",
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

    val subject = ByteStr.decodeBase58("264h1cUrahDxWCPJBAPgtf6A9f3dNhkrLAeBUdHU8A5NDtksaumZ4WmsAU2NiF4eTCubLpYAd9D6xgBosPv34inu").get

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))

    val proofs = Seq(
      ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get,
      ByteStr.decodeBase58("2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx").get
    )
    val tx = IssueAssociationTransaction
      .create(
        version = 4,
        chainId = None,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        recipient = Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        assocType = 420,
        subject = Some(subject),
        fee = 100000,
        timestamp = 1526911531530L,
        expires = Some(1558447531530L),
        data = List(entry1, entry2, entry3),
        sponsor = Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        proofs = Proofs(proofs)
      )
      .explicitGet()

    tx.json() shouldEqual js
  }
}
