package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.api.http.requests.LeaseRequest
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.utils.Base58
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

class LeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs

    first.bytes() shouldEqual second.bytes()
  }

  property("Lease transaction serialization roundtrip versions") {
    forEvery(versionTable(LeaseTransaction)) { version =>
      forAll(leaseGen(version)) { tx: LeaseTransaction =>
        val recovered = tx.builder.parseBytes(tx.bytes()).get
        assertTxs(recovered, tx)
      }
    }
  }

  property("Lease transaction serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(leaseGen(3.toByte, keyType)) { tx: LeaseTransaction =>
        val recovered = tx.builder.parseBytes(tx.bytes()).get
        assertTxs(recovered, tx)
      }
    }
  }

  property("Lease transaction from TransactionBuilder") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  property("JSON roundtrip") {
    forAll(leaseGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[LeaseRequest]
      req.senderPublicKey should be ('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp should be ('defined)
      req.timestamp.get shouldEqual tx.timestamp

      req.recipient shouldEqual tx.recipient.toString
      req.amount shouldEqual tx.amount
    }
  }

  property("JSON format validation for LeaseTransaction V1") {
    val js = Json.parse("""{
                       "type": 8,
                       "version": 1,
                       "id": "7EyfHdDiassBQ5ZAyXKefW4743A4HqHSB6DHirVmCUkv",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 1000000,
                       "timestamp": 1526646300260,
                       "amount": 10000000,
                       "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "signature": "iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ"
                     }
    """)

    val tx = LeaseTransaction
      .create(
        version = 1,
        chainId = None,
        timestamp = 1526646300260L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 1000000,
        recipient = Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        amount = 10000000,
        sponsor = None,
        proofs = Proofs.fromSignature(ByteStr.decodeBase58("iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ").get)
      ).explicitGet()

    tx.json() shouldEqual js
  }

  property("JSON format validation for LeaseTransaction V2") {
    val js = Json.parse("""{
                        "type": 8,
                        "version": 2,
                        "id": "3EuU5xQrkA6juSGHszb8TJgxbfmoz6Bdrcvu8HQuu2dT",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderKeyType": "ed25519",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "timestamp": 1526646497465,
                        "amount": 10000000,
                        "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                        "proofs": [
                          "5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q"
                        ]
                        }
    """)

    val tx = LeaseTransaction
      .create(
        version = 2,
        chainId = None,
        timestamp = 1526646497465L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 1000000,
        recipient = Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        amount = 10000000,
        sponsor = None,
        proofs = Proofs(Seq(ByteStr.decodeBase58("5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q").get))
      ).explicitGet()

    tx.json() shouldEqual js
  }

  property("JSON format validation for LeaseTransaction V3") {
    val js = Json.parse("""{
                        "type": 8,
                        "version": 3,
                        "id": "EGmBEd2Gy56PbThvUNLMvvt6MQNXgaMh2B3s3TT7r5yX",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderKeyType": "ed25519",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                        "sponsorKeyType": "ed25519",
                        "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                        "fee": 1000000,
                        "timestamp": 1526646497465,
                        "amount": 10000000,
                        "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
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

    val tx = LeaseTransaction
      .create(
        version = 3,
        chainId = None,
        timestamp = 1526646497465L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 1000000,
        recipient = Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        amount = 10000000,
        sponsor = Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        proofs = Proofs(proofs)
      ).explicitGet()

    tx.json() shouldEqual js
  }

}
