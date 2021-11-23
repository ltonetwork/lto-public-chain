package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.lease.CancelLeaseTransaction
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.Inspectors.forEvery
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import play.api.libs.json.Json

class CancelLeaseTransactionSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen {

  private def assertTxs(first: CancelLeaseTransaction, second: CancelLeaseTransaction): Unit = {
    first.leaseId shouldEqual second.leaseId
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs

    first.bytes() shouldEqual second.bytes()
  }

  property("Cancel lease serialization roundtrip versions") {
    forEvery(versionTable(CancelLeaseTransaction)) { version =>
      forAll(cancelLeaseGen(version)) { tx: CancelLeaseTransaction =>
        val recovered = tx.builder.parseBytes(tx.bytes()).get
        assertTxs(recovered, tx)
      }
    }
  }

  property("Cancel lease serialization roundtrip keypairs") {
    forEvery(keyTypeTable) { keyType =>
      forAll(cancelLeaseGen(3.toByte, keyType)) { tx: CancelLeaseTransaction =>
        val recovered = tx.builder.parseBytes(tx.bytes()).get
        assertTxs(recovered, tx)
      }
    }
  }

  property("Cancel lease serialization from TypedTransaction") {
    forAll(cancelLeaseGen) { tx: CancelLeaseTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[CancelLeaseTransaction], tx)
    }
  }

  property("JSON format validation for CancelLeaseTransaction V1") {
    val js = Json.parse("""{
                       "type": 9,
                       "version": 1,
                       "id": "7hmabbFS8a2z79a29pzZH1s8LHxrsEAnnLjJxNdZ1gGw",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 1000000,
                       "timestamp": 1526646300260,
                       "leaseId": "EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY",
                       "signature": "4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3"
                       }
    """)

    val tx = CancelLeaseTransaction
      .create(
        version = 1,
        chainId = None,
        timestamp = 1526646300260L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 1000000,
        leaseId = ByteStr.decodeBase58("EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY").get,
        sponsor = None,
        proofs =
          Proofs.fromSignature(ByteStr.decodeBase58("4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3").get)
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

  property("JSON format validation for CancelLeaseTransaction V2") {
    val js = Json.parse("""{
                        "type": 9,
                        "version": 2,
                        "id": "4nvUUiQjTH7D2LFyzaxs8JwaZYZHDggJgq1iP99TvVDM",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderKeyType": "ed25519",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "timestamp": 1526646300260,
                        "leaseId": "DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6",
                        "proofs": [
                          "3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi"
                        ]
                       }
    """)

    val tx = CancelLeaseTransaction
      .create(
        version = 2,
        chainId = None,
        timestamp = 1526646300260L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 1000000,
        leaseId = ByteStr.decodeBase58("DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6").get,
        sponsor = None,
        proofs = Proofs(Seq(ByteStr.decodeBase58("3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi").get))
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

  property("JSON format validation for CancelLeaseTransaction V3") {
    val js = Json.parse("""{
                        "type": 9,
                        "version": 3,
                        "id": "4WXJ6EnavyNLp5aov6bS63wmrpKG75d1hqHHXNGtM2KK",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderKeyType": "ed25519",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                        "sponsorKeyType": "ed25519",
                        "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                        "fee": 1000000,
                        "timestamp": 1526646300260,
                        "leaseId": "DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6",
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

    val tx = CancelLeaseTransaction
      .create(
        version = 3,
        chainId = None,
        timestamp = 1526646300260L,
        sender = PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        fee = 1000000,
        leaseId = ByteStr.decodeBase58("DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6").get,
        sponsor = Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        proofs = Proofs(proofs)
      )
      .explicitGet()

    tx.json() shouldEqual js
  }

}
