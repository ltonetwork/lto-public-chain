package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.api.http.requests.{CancelSponsorshipRequest, SponsorshipRequest}
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
    forEvery(versionTable(SponsorshipTransaction)) { version =>
      forAll(sponsorshipGen(version))(tx => checkSerialization(tx, SponsorshipTransaction.parseBytes))
      forAll(cancelSponsorshipGen(version))(tx => checkSerialization(tx, CancelSponsorshipTransaction.parseBytes))
    }
  }

  property("serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(sponsorshipGen(3.toByte, keyType))(tx => checkSerialization(tx, SponsorshipTransaction.parseBytes))
      forAll(cancelSponsorshipGen(3.toByte, keyType))(tx => checkSerialization(tx, CancelSponsorshipTransaction.parseBytes))
    }
  }

  property("from TransactionBuilder") {
    forAll(sponsorshipGen) { tx: SponsorshipTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }

    forAll(cancelSponsorshipGen) { tx: CancelSponsorshipTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip sponsorship") {
    forAll(sponsorshipGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SponsorshipRequest]
      req.senderPublicKey should be ('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be ('defined)
      req.timestamp.get shouldEqual tx.timestamp
      req.recipient shouldEqual tx.recipient.stringRepr
    }
  }

  property("JSON roundtrip cancel sponsorship") {
    forAll(cancelSponsorshipGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[CancelSponsorshipRequest]
      req.senderPublicKey should be ('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be ('defined)
      req.timestamp.get shouldEqual tx.timestamp
      req.recipient shouldEqual tx.recipient.stringRepr
    }
  }

  property(testName = "JSON format validation V1") {
    val types = Table(
      ("type", "id", "create"),
      (18, "7ijEThzAq5ANyT15PwZgjr92HaRdy412AGTkPGPvfVAv", SponsorshipTransaction.create _),
      (19, "3SUXAaKh5ayCsQLp13MHqmmt8ste9ArsvshiKBzDMMm3", CancelSponsorshipTransaction.create _)
    )

    forEvery(types) { (typeId, id, create) =>
      val js = Json.parse(
        s"""{
                       "type": $typeId,
                       "version": 1,
                       "id": "$id",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "recipient" : "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val arr = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get
    val tx = create(
        1,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        None,
        Proofs(Seq(arr))
      )
      .explicitGet()

      tx.json() shouldEqual js
    }
  }

  property(testName = "JSON format validation V3") {
    val types = Table(
      ("type", "id", "create"),
      (18, "29JRUpT6uBSbmpUeu4tazgA4VLswX86AzrkukKr2jM2E", SponsorshipTransaction.create _),
      (19, "5D8wCnhMzuyMnSxdSL8gwtdurBPqayN1kyci7i8yPfhx", CancelSponsorshipTransaction.create _)
    )

    forEvery(types) { (typeId, id, create) =>
      val js = Json.parse(
        s"""{
                       "type": $typeId,
                       "version": 3,
                       "id": "$id",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "recipient" : "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94",
                         "2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx"
                       ]
                       }
  """)

      val proofs = Seq(
        ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get,
        ByteStr.decodeBase58("2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx").get
      )
      val tx = create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        Proofs(proofs)
      )
        .explicitGet()

      tx.json() shouldEqual js
    }
  }
}
