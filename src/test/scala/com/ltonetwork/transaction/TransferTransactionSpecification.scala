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

  private def checkSerialization(tx: TransferTransaction): Assertion = {
    val recovered = TransferTransaction.parseBytes(tx.bytes()).get

    recovered.version shouldEqual tx.version
    recovered.chainId shouldEqual tx.chainId
    recovered.sender shouldEqual tx.sender
    recovered.timestamp shouldEqual tx.timestamp
    recovered.amount shouldEqual tx.amount
    recovered.fee shouldEqual tx.fee
    recovered.recipient shouldEqual tx.recipient
    recovered.attachment shouldEqual tx.attachment
    recovered.sponsor shouldEqual tx.sponsor

    recovered.bytes() shouldEqual tx.bytes()
  }

  property("Transfer serialization roundtrip") {
    forEvery(versionTable(TransferTransaction)) { version =>
      forAll(transferGen(version))(checkSerialization)
    }
  }

  property("Transfer serialization from TransactionBuilders") {
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

  private val fastIdVersionGen: Gen[Byte] = Gen.oneOf(TransferTransaction.supportedVersions.toSeq) filter (_ != 1)

  property("TransferTransaction >= v2 id doesn't depend on proof") {
    forAll(fastIdVersionGen, accountGen, accountGen, proofsGen, proofsGen, bytes32gen) {
      case (version, acc1, acc2, proofs1, proofs2, attachment) =>
        val tx1 = TransferTransaction.create(version, None, 1, acc1, 1, acc2.toAddress, 1, attachment, None, proofs1).explicitGet()
        val tx2 = TransferTransaction.create(version, None, 1, acc1, 1, acc2.toAddress, 1, attachment, None, proofs2).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  property("JSON format validation V1") {
    val js = Json.parse("""{
                        "type": 4,
                        "version": 1,
                        "id": "512j4tvEqhkuDh8tk9AwbUFUDUeRhyjkKvJTog3q9jtM",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderKeyType": "ed25519",
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
      ).explicitGet()

    tx.json() shouldEqual js
  }

  property("JSON format validation V2") {
    val js = Json.parse("""{
                       "type": 4,
                       "version": 2,
                       "id": "2sYxwfjUWAcJuDThgdaMRk4z3vpmzs3qhhuNb6sBA8JX",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
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
      ).explicitGet()

    tx.json() shouldEqual js
  }

  property("JSON format validation V3") {
    val js = Json.parse("""{
                       "type": 4,
                       "version": 3,
                       "id": "Hvd5tfDHYgyKKextoVE5VkxGDW8nCCtiX98dgKe3GAFz",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "sponsor": "3Mw6BfpSRkgCi8LQMQRKayvEb1fqKpDbaVY",
                       "sponsorKeyType": "ed25519",
                       "sponsorPublicKey": "22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz",
                       "fee": 100000000,
                       "timestamp": 1526641218066,
                       "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                       "amount": 100000000,
                       "attachment": "4t2Xazb2SX",
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

    val tx = TransferTransaction
      .create(
        3,
        None,
        1526641218066L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000000,
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        100000000,
        Base58.decode("4t2Xazb2SX").get,
        Some(PublicKeyAccount.fromBase58String("22wYfvU2op1f3s4RMRL2bwWBmtHCAB6t3cRwnzRJ1BNz").explicitGet()),
        Proofs(proofs)
      ).explicitGet()

    tx.json() shouldEqual js
  }
}
