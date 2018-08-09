package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.state.diffs._
import com.wavesplatform.utils.Base58

class TransferTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferV1Gen) { transfer: TransferTransactionV1 =>
      val recovered = TransferTransactionV1.parseBytes(transfer.bytes()).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferV1Gen) { tx: TransferTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                        "type": 4,
                        "id": "6e3LbdveBEAGbNX2zYuAHXH2kpqzsZsk1RbNR2F6TVaH",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 100000,
                        "timestamp": 1526552510868,
                        "signature": "eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz",
                        "version": 1,
                        "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                        "feeAsset": null,
                        "amount": 1900000,
                        "attachment": "4t2Xazb2SX"
                        }
    """)

    val tx = TransferTransactionV1
      .create(
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromString("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt").explicitGet(),
        1900000,
        1526552510868L,
        100000,
        Base58.decode("4t2Xazb2SX").get,
        ByteStr.decodeBase58("eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz").get
      )
      .right
      .get

    tx.json() shouldEqual js
  }

  property("negative") {
    for {
      (sender, recipient, amount, timestamp, feeAmount, attachment) <- transferParamGen
      sender                                                        <- accountGen
    } yield TransferTransactionV1.selfSigned(sender, recipient, amount, timestamp, feeAmount, attachment) should produce("insufficient fee")
  }
}
