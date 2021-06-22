package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.state.diffs._
import com.ltonetwork.utils.Base58
import org.scalacheck.Gen

class TransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferGen) { transfer: TransferTransaction =>
      val recovered = TransferTransaction.parseBytes(transfer.bytes()).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferGen) { tx: TransferTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("negative") {
    for {
      (_, recipient, amount, timestamp, fee, attachment) <- transferParamGen
      sender                                             <- accountGen
    } yield TransferTransaction.selfSigned(1, timestamp, sender, fee, recipient, amount, attachment) should produce("insufficient fee")
  }

  private val versionGen: Gen[Byte] = Gen.oneOf(TransferTransaction.supportedVersions.toSeq) filter(_ != 1)

  property("TransferTransaction v2 id doesn't depend on proof") {
    forAll(versionGen, accountGen, accountGen, proofsGen, proofsGen, bytes32gen) {
      case (version, acc1, acc2, proofs1, proofs2, attachment) =>
        val tx1 = TransferTransaction.create(version, None, 1, acc1, 1, acc2.toAddress, 1, attachment, None, proofs1).explicitGet()
        val tx2 = TransferTransaction.create(version, None, 1, acc1, 1, acc2.toAddress, 1, attachment, None, proofs2).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: TransferTransaction, second: TransferTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.amount shouldEqual second.amount
    first.recipient shouldEqual second.recipient
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation V1") {
    val js = Json.parse("""{
                        "type": 4,
                        "version": 1,
                        "id": "512j4tvEqhkuDh8tk9AwbUFUDUeRhyjkKvJTog3q9jtM",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 100000,
                        "timestamp": 1526552510868,
                        "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                        "amount": 1900000,
                        "attachment": "4t2Xazb2SX",
                        "signature": "eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz"
                        }
    """)

    val tx = TransferTransaction
      .create(
        1,
        None,
        1526552510868L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        1900000,
        Base58.decode("4t2Xazb2SX").get,
        None,
        Proofs.fromSignature(ByteStr.decodeBase58("eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz").get)
      )
      .right
      .get

    tx.json() shouldEqual js
  }

  property("JSON format validation V2") {
    val js = Json.parse("""{
                       "type": 4,
                       "version": 2,
                       "id": "2sYxwfjUWAcJuDThgdaMRk4z3vpmzs3qhhuNb6sBA8JX",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526641218066,
                       "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "amount": 100000000,
                       "attachment": "4t2Xazb2SX",
                       "proofs": [
                       "4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi"
                       ]}
    """)

    val tx = TransferTransaction
      .create(
        2,
        None,
        1526641218066L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000000,
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        100000000,
        Base58.decode("4t2Xazb2SX").get,
        None,
        Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get))
      )
      .right
      .get

    tx.json() shouldEqual js
  }
}
