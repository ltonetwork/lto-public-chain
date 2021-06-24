package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.smart.SignedSetScriptV1Request
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.utils.Base58
import com.ltonetwork.transaction.smart.script.ScriptCompilerV1Test
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}

class SetScriptTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {
  property("serialization roundtrip") {
    forAll(setScriptTransactionGen) { tx: SetScriptTransaction =>
      val parsed = SetScriptTransaction.parseBytes(tx.bytes()).get

      parsed.sender.address shouldEqual tx.sender.address
      parsed.timestamp shouldEqual tx.timestamp
      parsed.fee shouldEqual tx.fee
      parsed.script shouldEqual tx.script

      parsed.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedSetScriptV1Request] = Json.format[SignedSetScriptV1Request]

    forAll(setScriptTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedSetScriptV1Request]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp shouldEqual tx.timestamp

      req.script shouldEqual tx.script.map(_.bytes().base64)
    }
  }

  property(testName = "JSON format validation") {
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
        Some(ScriptCompilerV1Test.compiledScript),
        None,
        Proofs(Seq(arr))
      )
      .right
      .get

    tx.json() shouldEqual js
  }

}
