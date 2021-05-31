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

class TransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferV1Gen) { transfer: TransferTransaction =>
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
    forAll(transferV1Gen) { tx: TransferTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                        "type": 4,
                        "id": "512j4tvEqhkuDh8tk9AwbUFUDUeRhyjkKvJTog3q9jtM",
                        "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 100000,
                        "timestamp": 1526552510868,
                        "signature": "eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz",
                        "version": 1,
                        "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                        "amount": 1900000,
                        "attachment": "4t2Xazb2SX"
                        }
    """)

    val tx = TransferTransaction
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
    } yield TransferTransaction.selfSigned(sender, recipient, amount, timestamp, feeAmount, attachment) should produce("insufficient fee")
  }
}
