package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.Base58
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

class TransferTransactionV2Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private val versionGen: Gen[Byte] = Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)

  property("VersionedTransferTransactionSpecification serialization roundtrip") {
    forAll(transferV2Gen) { tx: TransferTransactionV2 =>
      val recovered = TransferTransactionV2.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("VersionedTransferTransactionSpecification serialization from TypedTransaction") {
    forAll(transferV2Gen) { tx: TransferTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[TransferTransactionV2], tx)
    }
  }

  property("VersionedTransferTransactionSpecification id doesn't depend on proof") {
    forAll(versionGen, accountGen, accountGen, proofsGen, proofsGen, bytes32gen) {
      case (version, acc1, acc2, proofs1, proofs2, attachment) =>
        val tx1 = TransferTransactionV2.create(version, acc2, acc2.toAddress, 1, 1, 1, attachment, proofs1).explicitGet()
        val tx2 = TransferTransactionV2.create(version, acc2, acc2.toAddress, 1, 1, 1, attachment, proofs2).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: TransferTransactionV2, second: TransferTransactionV2): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.amount shouldEqual second.amount
    first.recipient shouldEqual second.recipient
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 4,
                       "id": "2sYxwfjUWAcJuDThgdaMRk4z3vpmzs3qhhuNb6sBA8JX",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526641218066,
                       "proofs": [
                       "4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi"
                       ],
                       "version": 2,
                       "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "amount": 100000000,
                       "attachment": "4t2Xazb2SX"}
    """)

    val tx = TransferTransactionV2
      .create(
        2,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        100000000,
        1526641218066L,
        100000000,
        Base58.decode("4t2Xazb2SX").get,
        Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get))
      )
      .right
      .get

    tx.json() shouldEqual js
  }
}