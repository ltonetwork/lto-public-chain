package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.requests.BurnRequest
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.burn.BurnTransaction
import com.ltonetwork.utils.Base58
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

class BurnTransactionSpecification
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with TableDrivenPropertyChecks
    with Matchers
    with TransactionGen {

  private def checkSerialization(tx: BurnTransaction): Assertion = {
    val tryParse = BurnTransaction.parseBytes(tx.bytes())
    tryParse should be a 'success

    val parsed = tryParse.get
    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.sponsor shouldEqual tx.sponsor
    parsed.amount shouldEqual tx.amount

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip versions") {
    forEvery(versionTable(BurnTransaction)) { version: Byte =>
      forAll(burnTransactionGen(version))(checkSerialization)
    }
  }

  property("serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(burnTransactionGen(3.toByte, keyType))(checkSerialization)
    }
  }

  property("from TransactionBuilder") {
    forAll(burnTransactionGen) { tx: BurnTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(burnTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[BurnRequest]
      req.senderPublicKey should be('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be('defined)
      req.timestamp.get shouldEqual tx.timestamp

      req.amount shouldEqual tx.amount
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
          "type": 21,
          "version": 3,
          "id": "9xoJrR7VyM7hT5o33QejM8gBgdDi92adkMiKWvA4caAB",
          "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
          "senderKeyType": "ed25519",
          "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
          "fee": 100000,
          "timestamp": 1526911531530,
          "amount": 5000000,
          "proofs": ["32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"]
      }
    """)

    val proof = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get

    val tx = BurnTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        5000000,
        None,
        Proofs(Seq(proof))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

  property(testName = "JSON format validation for sponsored tx") {
    val js = Json.parse("""{
          "type"  : 21,
          "version": 3,
          "id": "9xoJrR7VyM7hT5o33QejM8gBgdDi92adkMiKWvA4caAB",
          "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
          "senderKeyType": "ed25519",
          "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
          "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
          "sponsorKeyType": "ed25519",
          "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
          "fee": 100000,
          "timestamp": 1526911531530,
          "amount": 5000000,
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

    val tx = BurnTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        5000000,
        Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        Proofs(proofs)
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
