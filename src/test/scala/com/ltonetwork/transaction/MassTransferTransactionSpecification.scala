package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.state.{ByteStr, EitherExt2}
import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.ltonetwork.account.{AddressScheme, PublicKeyAccount}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer, Transfer}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.Base58

class MassTransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("serialization roundtrip") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      require(tx.bytes().head == MassTransferTransaction.typeId)
      val recovered = MassTransferTransaction.parseBytes(tx.bytes()).get

      recovered.sender.address shouldEqual tx.sender.address
      recovered.timestamp shouldEqual tx.timestamp
      recovered.fee shouldEqual tx.fee

      recovered.transfers.zip(tx.transfers).foreach {
        case (ParsedTransfer(rr, ra), ParsedTransfer(tr, ta)) =>
          rr shouldEqual tr
          ra shouldEqual ta
      }

      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("serialization from TypedTransaction") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("property validation") {
    import MassTransferTransaction.create

    val badVersionGen = Arbitrary.arbByte.arbitrary.filter(x => !MassTransferTransaction.supportedVersions.contains(x))
    forAll(massTransferGen, badVersionGen) {
      case (MassTransferTransaction(version, networkByte, timestamp, sender, fee, transfers, attachment, sponsor, proofs), badVersion) =>
        val badVersionEi = create(badVersion, Some(networkByte), timestamp, sender, fee, transfers, attachment, sponsor, proofs)
        badVersionEi shouldBe Left(ValidationError.UnsupportedVersion(badVersion))

        val tooManyTransfers   = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, 1L))
        val tooManyTransfersEi = create(version, Some(networkByte), timestamp, sender, fee, tooManyTransfers, attachment, sponsor, proofs)
        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers is greater than $MaxTransferCount"))

        val negativeTransfer   = List(ParsedTransfer(sender.toAddress, -1L))
        val negativeTransferEi = create(version, Some(networkByte), timestamp, sender, fee, negativeTransfer, attachment, sponsor, proofs)
        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))

        val oneHalf    = Long.MaxValue / 2 + 1
        val overflow   = List.fill(2)(ParsedTransfer(sender.toAddress, oneHalf))
        val overflowEi = create(version, Some(networkByte), timestamp, sender, fee, overflow, attachment, sponsor, proofs)
        overflowEi shouldBe Left(ValidationError.OverflowError)

        val feeOverflow   = List(ParsedTransfer(sender.toAddress, oneHalf))
        val feeOverflowEi = create(version, Some(networkByte), timestamp, sender, oneHalf, feeOverflow, attachment, sponsor, proofs)
        feeOverflowEi shouldBe Left(ValidationError.OverflowError)

        val longAttachment   = Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte)
        val longAttachmentEi = create(version, Some(networkByte), timestamp, sender, fee, transfers, longAttachment, sponsor, proofs)
        longAttachmentEi shouldBe Left(ValidationError.TooBigArray)

        val noFeeEi = create(version, Some(networkByte), timestamp, sender, 0, transfers, attachment, sponsor, proofs)
        noFeeEi shouldBe Left(ValidationError.InsufficientFee())

        val negativeFeeEi = create(version, Some(networkByte), timestamp, sender, -100, transfers, attachment, sponsor, proofs)
        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee())
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 11,
                       "version": 1,
                       "id": "Dz3yJ9dKvm2A32HtWwDnUtFuJHURgfiPxNQpzP5HzNGU",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderKeyType": "ed25519",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 200000,
                       "timestamp": 1518091313964,
                       "attachment": "59QuUcqP6p",
                       "transferCount": 2,
                       "totalAmount": 300000000,
                       "transfers": [
                         {
                           "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                           "amount": 100000000
                         },
                         {
                           "recipient": "3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt",
                           "amount": 200000000
                         }
                       ],
                       "proofs": [
                         "FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ"
                       ]
                       }
  """)

    val transfers = MassTransferTransaction
      .parseTransfersList(
        List(Transfer("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt", 100000000L), Transfer("3N5XyVTp4kEARUGRkQTuCVN6XjV4c5iwcJt", 200000000L)))
      .right
      .get

    val tx = MassTransferTransaction
      .create(
        1,
        None,
        1518091313964L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        200000,
        transfers,
        Base58.decode("59QuUcqP6p").get,
        None,
        Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
      )
      .right
      .get

    tx.json() shouldEqual js
  }
}
