package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{KeyTypes, PublicKeyAccount}
import com.ltonetwork.api.requests.RegisterRequest
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.utils.Base58
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

class RegisterTransactionSpecification
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with TableDrivenPropertyChecks
    with Matchers
    with TransactionGen {

  private def checkSerialization(tx: RegisterTransaction): Assertion = {
    val tryParse = RegisterTransaction.parseBytes(tx.bytes())
    tryParse should be a 'success

    val parsed = tryParse.get
    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.sponsor shouldEqual tx.sponsor

    parsed.accounts.zip(tx.accounts).foreach {
      case (r, t) =>
        r shouldEqual t
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip versions") {
    forEvery(versionTable(RegisterTransaction)) { version: Byte =>
      forAll(registerTransactionGen(version))(checkSerialization)
    }
  }

  property("serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(registerTransactionGen(3.toByte, keyType))(checkSerialization)
    }
  }

  property("from TransactionBuilder") {
    forAll(registerTransactionGen) { tx: RegisterTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(registerTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[RegisterRequest]
      req.senderPublicKey should be('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be('defined)
      req.timestamp.get shouldEqual tx.timestamp

      req.accounts zip tx.accounts foreach {
        case (re, te) =>
          re.keyType shouldEqual te.toKey._1.toString
          re.publicKey shouldEqual te.toKey._2
      }
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 20,
                       "version": 3,
                       "id": "DB23XJSkkSLEjZtp8Q8VjjYUuxRQgdYGtd2E2x2smVt4",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "accounts": [
                         {
                           "keyType": "ed25519",
                           "publicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z"
                         }
                       ],
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val account = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet()
    val proof   = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get

    val tx = RegisterTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        List(account),
        None,
        Proofs(Seq(proof))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }


  property(testName = "JSON format validation with all key types") {
    val js = Json.parse("""{
                       "type": 20,
                       "version": 3,
                       "id": "FKqhStbC2YYdk8kK488tHCnBJ4Wfv1WbXvxDvmAJREfY",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "accounts": [
                         {
                           "keyType": "ed25519",
                           "publicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z"
                         },
                         {
                           "keyType": "secp256k1",
                           "publicKey": "u4udut3StdCe7jCqGPW59KorcnFXkyVrH36WCJb6J4ERP"
                         },
                         {
                           "keyType": "secp256r1",
                           "publicKey": "VXEHgucYi9AZ8mZRU3KKKuXpoVgzWNzoiD8HPiC5eqp8H"
                         },
                         {
                           "keyType": "bls12-381",
                           "publicKey": "81SKPmARHUvSUZDc821CVzGEcfoXWXT7b8ZoVCimUDqwcoE5zj4T2sPpjBuu3zpXZS"
                         }
                       ],
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ]
                       }
  """)

    val accounts = List(
      PublicKeyAccount.fromBase58String(KeyTypes.ED25519, "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      PublicKeyAccount.fromBase58String(KeyTypes.SECP256K1, "u4udut3StdCe7jCqGPW59KorcnFXkyVrH36WCJb6J4ERP").explicitGet(),
      PublicKeyAccount.fromBase58String(KeyTypes.SECP256R1, "VXEHgucYi9AZ8mZRU3KKKuXpoVgzWNzoiD8HPiC5eqp8H").explicitGet(),
      PublicKeyAccount.fromBase58String(KeyTypes.BLS12_381, "81SKPmARHUvSUZDc821CVzGEcfoXWXT7b8ZoVCimUDqwcoE5zj4T2sPpjBuu3zpXZS").explicitGet(),
    )

    val proof = ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get

    val tx = RegisterTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        accounts,
        None,
        Proofs(Seq(proof))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

  property(testName = "JSON format validation for sponsored tx") {
    val js = Json.parse("""{
                       "type": 20,
                       "version": 3,
                       "id": "7yerk1cuwCpotz9xG1Mm9yfk2TPPQmVvnJ7xVJbH3DfL",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "accounts": [
                         {
                           "keyType": "ed25519",
                           "publicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z"
                         },
                         {
                           "keyType": "ed25519",
                           "publicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz"
                         }
                       ],
                       "proofs": [
                         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94",
                         "2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx"
                       ]
                       }
  """)

    val accounts = List(
      PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()
    )

    val proofs = Seq(
      ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get,
      ByteStr.decodeBase58("2z2S3W9n9AatLQ4XmR5mPfZdGY3o27JY7Bf9c7GeD3GDhGykxuSEjKMkwh2yALDcBhdduFGLT1pXJww4Dg6eMHRx").get
    )

    val tx = RegisterTransaction
      .create(
        3,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        accounts,
        Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        Proofs(proofs)
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
