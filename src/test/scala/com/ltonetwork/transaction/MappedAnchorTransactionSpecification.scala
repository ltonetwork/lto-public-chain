package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.requests.MappedAnchorRequest
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.anchor.MappedAnchorTransaction
import com.ltonetwork.utils.Base58
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

class MappedAnchorTransactionSpecification
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with TableDrivenPropertyChecks
    with Matchers
    with TransactionGen {

  private def checkSerialization(tx: MappedAnchorTransaction): Assertion = {
    val tryParse = MappedAnchorTransaction.parseBytes(tx.bytes())
    tryParse should be a 'success

    val parsed = tryParse.get
    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.sponsor shouldEqual tx.sponsor

    parsed.anchors.zip(tx.anchors).foreach {
      case ((rk, rv), (tk, tv)) =>
        rk shouldEqual tk
        rv shouldEqual tv
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip versions") {
    forEvery(versionTable(MappedAnchorTransaction)) { version: Byte =>
      forAll(mappedAnchorTransactionGen(version))(checkSerialization)
    }
  }

  property("serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(mappedAnchorTransactionGen(3.toByte, keyType))(checkSerialization)
    }
  }

  property("from TransactionBuilder") {
    forAll(mappedAnchorTransactionGen) { tx: MappedAnchorTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(mappedAnchorTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[MappedAnchorRequest]
      req.senderPublicKey should be('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be('defined)
      req.timestamp.get shouldEqual tx.timestamp

      req.anchors.toSeq should have length(tx.anchors.toSeq.length)

      tx.anchors foreach {
        case (k, v) => req.anchors should contain((k.base58, v.base58))
      }
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 22,
                       "version": 3,
                       "id": "C6JegZfghBDvoQy5JMNUu6cQJqp7ueGaYqng1tmDNrKg",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "anchors": {
                         "5Fofboiini74ercUkMBEva": "173n9Z2psrx1r69DT2136U"
                       },
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val anchors = Map(ByteStr.decodeBase58("5Fofboiini74ercUkMBEva").get -> ByteStr.decodeBase58("173n9Z2psrx1r69DT2136U").get)
    val proof   = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get

    val tx = MappedAnchorTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        anchors,
        None,
        Proofs(Seq(proof))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

  property(testName = "JSON format validation for sponsored tx") {
    val js = Json.parse("""{
                       "type": 22,
                       "version": 3,
                       "id": "BdWK68oPTvx8wMgyGGdRTneAkzRLTMYi9W5JQ4XWFn6A",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "anchors": {
                         "5Fofboiini74ercUkMBEva": "173n9Z2psrx1r69DT2136U",
                         "H92afmSEphgqSHpCXWFCLM": "VhBfCKmrRCYL4cZxDCT6G2"
                       },
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94",
                         "2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx"
                       ]
                       }
  """)

    val anchors = Map(
      ByteStr.decodeBase58("5Fofboiini74ercUkMBEva").get -> ByteStr.decodeBase58("173n9Z2psrx1r69DT2136U").get,
      ByteStr.decodeBase58("H92afmSEphgqSHpCXWFCLM").get -> ByteStr.decodeBase58("VhBfCKmrRCYL4cZxDCT6G2").get
    )

    val proofs = Seq(
      ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get,
      ByteStr.decodeBase58("2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx").get
    )

    val tx = MappedAnchorTransaction
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
