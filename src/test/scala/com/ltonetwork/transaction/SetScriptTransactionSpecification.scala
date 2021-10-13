package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.SetScriptRequest
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.ScriptCompilerV1Test.compiledScript
import com.ltonetwork.utils.Base58
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

class SetScriptTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {
  property("serialization roundtrip versions") {
    forEvery(versionTable(SetScriptTransaction)) { version =>
      forAll(setScriptTransactionGen(version)) { tx: SetScriptTransaction =>
        val parsed = SetScriptTransaction.parseBytes(tx.bytes()).get

        parsed.sender.address shouldEqual tx.sender.address
        parsed.timestamp shouldEqual tx.timestamp
        parsed.fee shouldEqual tx.fee
        parsed.script shouldEqual tx.script

        parsed.bytes() shouldEqual tx.bytes()
      }
    }
  }

  property("serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(setScriptTransactionGen(3.toByte, keyType)) { tx: SetScriptTransaction =>
        val parsed = SetScriptTransaction.parseBytes(tx.bytes()).get

        parsed.sender.address shouldEqual tx.sender.address
        parsed.timestamp shouldEqual tx.timestamp
        parsed.fee shouldEqual tx.fee
        parsed.script shouldEqual tx.script

        parsed.bytes() shouldEqual tx.bytes()
      }
    }
  }

  property("from TransactionBuilder") {
    forAll(setScriptTransactionGen) { tx: SetScriptTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(setScriptTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SetScriptRequest]
      req.senderPublicKey should be ('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be ('defined)
      req.timestamp.get shouldEqual tx.timestamp

      req.script shouldEqual tx.script.map(_.bytes().base64)
    }
  }

  property(testName = "JSON format validation V1") {
    val js = Json.parse("""{
                       "type": 13,
                       "version": 1,
                       "id": "3ivr43eyGGsy5Grcy2kDUFmruEq7bQZdvGaPpakXnrfr",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "script": "base64:AQQAAAABeAAAAAAAAAAACgkAAAAAAAACAAAAAAAAAAAUCQAAZAAAAAIFAAAAAXgFAAAAAXgY49ib",
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val arr = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get
    val tx = SetScriptTransaction
      .create(
        1,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        Some(compiledScript),
        None,
        Proofs(Seq(arr))
      )
      .right
      .get

    tx.json() shouldEqual js
  }


  property(testName = "JSON format validation V3") {
    val js = Json.parse("""{
                       "type": 13,
                       "version": 3,
                       "id": "FF1oXmxVaEHv7vw4JmQrv23kKqSfmuVccLLBxJof9cWx",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "script": "base64:AQQAAAABeAAAAAAAAAAACgkAAAAAAAACAAAAAAAAAAAUCQAAZAAAAAIFAAAAAXgFAAAAAXgY49ib",
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
    val tx = SetScriptTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        Some(compiledScript),
        Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        Proofs(proofs)
      )
      .right
      .get

    tx.json() shouldEqual js
  }
}
