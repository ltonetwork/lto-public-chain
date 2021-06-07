package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, CancelLeaseTransactionV1, CancelLeaseTransactionV2}

class CancelLeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease cancel serialization roundtrip") {
    forAll(leaseCancelGen) { tx: CancelLeaseTransaction =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get.asInstanceOf[CancelLeaseTransaction]
      assertTxs(recovered, tx)
    }
  }

  property("Lease cancel serialization from TypedTransaction") {
    forAll(leaseCancelGen) { tx: CancelLeaseTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[CancelLeaseTransaction], tx)
    }
  }

  private def assertTxs(first: CancelLeaseTransaction, second: CancelLeaseTransaction): Unit = {
    first.leaseId shouldEqual second.leaseId
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseCancelTransactionV1") {
    val js = Json.parse("""{
                       "type": 9,
                       "id": "7hmabbFS8a2z79a29pzZH1s8LHxrsEAnnLjJxNdZ1gGw",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 1000000,
                       "timestamp": 1526646300260,
                       "signature": "4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3",
                       "version": 1,
                       "leaseId": "EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY",
                       "chainId": null
                       }
    """)

    val tx = CancelLeaseTransactionV1
      .create(
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY").get,
        1000000,
        1526646300260L,
        ByteStr.decodeBase58("4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3").get
      )
      .right
      .get

    js shouldEqual tx.json()
  }

  property("JSON format validation for LeaseCancelTransactionV2") {
    val js = Json.parse("""{
                        "type": 9,
                        "id": "4nvUUiQjTH7D2LFyzaxs8JwaZYZHDggJgq1iP99TvVDM",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "timestamp": 1526646300260,
                        "proofs": [
                        "3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi"
                        ],
                        "version": 2,
                        "leaseId": "DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6",
                        "chainId": 84
                       }
    """)

    val tx = CancelLeaseTransactionV2
      .create(
        2,
        'T',
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6").get,
        1000000,
        1526646300260L,
        Proofs(Seq(ByteStr.decodeBase58("3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
