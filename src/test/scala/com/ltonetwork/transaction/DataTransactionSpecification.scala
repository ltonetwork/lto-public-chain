package com.ltonetwork.transaction

import com.google.common.primitives.Shorts
import com.ltonetwork.{TransactionGen, transaction}
import com.ltonetwork.state.DataEntry._
import com.ltonetwork.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, DataEntry, EitherExt2, IntegerDataEntry, StringDataEntry}
import com.ltonetwork.utils.Base58
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.data.SignedDataV1Request
import com.ltonetwork.transaction.data.DataTransaction
import scorex.crypto.encode.Base64

class DataTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: DataTransaction): Assertion = {
    val parsed = DataTransaction.parseBytes(tx.bytes()).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee

    parsed.data.zip(tx.data).foreach {
      case (r, t) =>
        r.key shouldEqual t.key
        r.value shouldEqual t.value
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(dataTransactionGen)(checkSerialization)
  }

  property("serialization from TypedTransaction") {
    forAll(dataTransactionGen) { tx: DataTransaction =>
      val recovered = DataTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("unknown type handing") {
    val badTypeIdGen = Gen.choose[Int](DataEntry.Type.maxId + 1, Byte.MaxValue)
    forAll(dataTransactionGen, badTypeIdGen) {
      case (tx, badTypeId) =>
        val bytes      = tx.bytes()
        val entryCount = Shorts.fromByteArray(bytes.drop(35))
        if (entryCount > 0) {
          val key1Length = Shorts.fromByteArray(bytes.drop(37))
          val p          = 39 + key1Length
          bytes(p) = badTypeId.toByte
          val parsed = DataTransaction.parseBytes(bytes)
          parsed.isFailure shouldBe true
          parsed.failed.get.getMessage shouldBe s"Unknown type $badTypeId"
        }
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedDataV1Request] = Json.format[SignedDataV1Request]

    forAll(dataTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedDataV1Request]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp shouldEqual tx.timestamp

      req.data zip tx.data foreach {
        case (re, te) =>
          re match {
            case BinaryDataEntry(k, v) =>
              k shouldEqual te.key
              v shouldEqual te.value
            case _: DataEntry[_] =>
              re shouldEqual te
            case _ => fail
          }
      }
    }
  }

  property("positive validation cases") {
    import DataTransaction.MaxEntryCount
    import com.ltonetwork.state._
    forAll(dataTransactionGen, dataEntryGen(500)) {
      case (DataTransaction(version, chainId, timestamp, sender, fee, data, sponsor, proofs), entry) =>
        def check(data: List[DataEntry[_]]): Assertion = {
          val txEi = DataTransaction.create(version, Some(chainId), timestamp, sender, fee, data, sponsor, proofs)
          txEi shouldBe Right(transaction.data.DataTransaction(version, chainId, timestamp, sender, fee, data, sponsor, proofs))
          checkSerialization(txEi.explicitGet())
        }

        check(List.empty)                                                               // no data
        check(List.tabulate(MaxEntryCount)(n => IntegerDataEntry(n.toString, n)))       // maximal data
        check(List.tabulate(30)(n => StringDataEntry(n.toString, "a" * 5109)))          // maximal data
        check(List(IntegerDataEntry("a" * MaxKeySize, 0xa)))                            // max key size
        check(List(BinaryDataEntry("bin", ByteStr.empty)))                              // empty binary
        check(List(BinaryDataEntry("bin", ByteStr(Array.fill(MaxValueSize)(1: Byte))))) // max binary value size
        check(List(StringDataEntry("str", "")))                                         // empty string
        check(List(StringDataEntry("str", "A" * MaxValueSize))) // max string size
    }
  }

  property("negative validation cases") {
    val badVersionGen = Arbitrary.arbByte.arbitrary.filter(v => !DataTransaction.supportedVersions.contains(v))
    forAll(dataTransactionGen, badVersionGen) {
      case (DataTransaction(version, chainId, timestamp, sender, fee, data, sponsor, proofs), badVersion) =>
        val badVersionEi = DataTransaction.create(badVersion, Some(chainId), timestamp, sender, fee, data, sponsor, proofs)
        badVersionEi shouldBe Left(ValidationError.UnsupportedVersion(badVersion))

        val dataTooBig   = List.tabulate(100)(n => StringDataEntry((100 + n).toString, "a" * 1527))
        val dataTooBigEi = DataTransaction.create(version, Some(chainId), timestamp, sender, fee, dataTooBig, sponsor, proofs)
        dataTooBigEi shouldBe Left(ValidationError.TooBigArray)

        val emptyKey   = List(IntegerDataEntry("", 2))
        val emptyKeyEi = DataTransaction.create(version, Some(chainId), timestamp, sender, fee, emptyKey, sponsor, proofs)
        emptyKeyEi shouldBe Left(ValidationError.GenericError("Empty key found"))

        val keyTooLong   = data :+ BinaryDataEntry("a" * (MaxKeySize + 1), ByteStr(Array(1, 2)))
        val keyTooLongEi = DataTransaction.create(version, Some(chainId), timestamp, sender, fee, keyTooLong, sponsor, proofs)
        keyTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val valueTooLong   = data :+ BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1: Byte)))
        val valueTooLongEi = DataTransaction.create(version, Some(chainId), timestamp, sender, fee, valueTooLong, sponsor, proofs)
        valueTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val e               = BooleanDataEntry("dupe", true)
        val duplicateKeys   = e +: data.drop(3) :+ e
        val duplicateKeysEi = DataTransaction.create(version, Some(chainId), timestamp, sender, fee, duplicateKeys, sponsor, proofs)
        duplicateKeysEi shouldBe Left(ValidationError.GenericError("Duplicate keys found"))

        val noFeeEi = DataTransaction.create(version, Some(chainId), timestamp, sender, 0, data, sponsor, proofs)
        noFeeEi shouldBe Left(ValidationError.InsufficientFee())

        val negativeFeeEi = DataTransaction.create(version, Some(chainId), timestamp, sender, -100, data, sponsor, proofs)
        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee())
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 12,
                       "id": "87SfuGJXH1cki2RGDH7WMTGnTXeunkc5mEjNKmmMdRzM",
                       "sender": "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526911531530,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 1,
                       "data": [
                       {
                       "key": "int",
                       "type": "integer",
                       "value": 24
                       },
                       {
                       "key": "bool",
                       "type": "boolean",
                       "value": true
                       },
                       {
                       "key": "blob",
                       "type": "binary",
                       "value": "base64:YWxpY2U="
                       }
                       ]
                       }
  """)

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val tx = DataTransaction
      .create(
        1,
        None,
        1526911531530L,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        100000,
        List(entry1, entry2, entry3),
        None,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    tx.json() shouldEqual js
  }

}
