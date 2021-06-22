package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.lease.LeaseTransaction
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

class LeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get.asInstanceOf[LeaseTransaction]
      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseTransaction V1") {
    val js = Json.parse("""{
                          |  "type": 8,
                          |  "id": "7EyfHdDiassBQ5ZAyXKefW4743A4HqHSB6DHirVmCUkv",
                          |  "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                          |  "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                          |  "fee": 1000000,
                          |  "timestamp": 1526646300260,
                          |  "signature": "iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ",
                          |  "version": 1,
                          |  "amount": 10000000,
                          |  "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt"
                          |}
    """.stripMargin)

    val tx = LeaseTransaction
      .create(
        1,
        None,
        1526646300260L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        1000000,
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        10000000,
        None,
        Proofs.fromSignature(ByteStr.decodeBase58("iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ").get)
      )
      .right
      .get

    tx.json() shouldEqual js
  }

  property("JSON format validation for LeaseTransaction V2") {
    val js = Json.parse("""{
                        "type": 8,
                        "id": "3EuU5xQrkA6juSGHszb8TJgxbfmoz6Bdrcvu8HQuu2dT",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "timestamp": 1526646497465,
                        "proofs": [
                        "5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q"
                        ],
                        "version": 2,
                        "amount": 10000000,
                        "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt"
                       }
    """)

    val tx = LeaseTransaction
      .create(
        2,
        None,
        1526646497465L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        1000000,
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        10000000,
        None,
        Proofs(Seq(ByteStr.decodeBase58("5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q").get))
      )
      .right
      .get

    tx.json() shouldEqual js
  }

}
