package com.ltonetwork.transaction.genesis

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state.{ByteStr, _}
import com.ltonetwork.transaction.TransactionBuilders._
import com.ltonetwork.transaction.TransactionParser.HardcodedVersion1
import com.ltonetwork.transaction.Transaction.SigProofsSwitch
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilder, TransactionSerializer, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class GenesisTransaction private (version: Byte, chainId: Byte, timestamp: Long, recipient: Address, amount: Long, proofs: Proofs)
    extends Transaction
    with SigProofsSwitch {

  override val fee                 = 0
  override val id: Coeval[ByteStr] = Coeval.evalOnce(signature)

  override def sender: PublicKeyAccount          = PublicKeyAccount.Dummy
  override def sponsor: Option[PublicKeyAccount] = None

  override def builder: TransactionBuilder.For[GenesisTransaction]      = GenesisTransaction
  private def serializer: TransactionSerializer.For[GenesisTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(Json.obj(
    "type"      -> GenesisTransaction.typeId,
    "version"   -> version,
    "id"        -> id().base58,
    "fee"       -> fee,
    "timestamp" -> timestamp,
    "signature" -> signature.base58,
    "recipient" -> recipient.address,
    "amount"    -> amount
  ))

  override protected def prefixByte: Coeval[Array[Byte]]  = Coeval.evalOnce(Array.emptyByteArray)
  override protected def footerBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array.emptyByteArray)
}

object GenesisTransaction extends TransactionBuilder.For[GenesisTransaction] {

  override val typeId: Byte                 = 1
  override val supportedVersions: Set[Byte] = Set(1)

  val RECIPIENT_LENGTH: Int = Address.AddressLength
  val BASE_LENGTH: Int      = TimestampLength + RECIPIENT_LENGTH + AmountLength

  override implicit def sign(tx: GenesisTransaction, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): GenesisTransaction =
    throw new Exception("Genesis transactions have a generated signature")

  override def serializer(version: Byte): TransactionSerializer.For[GenesisTransaction] = GenesisSerializer

  override def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = HardcodedVersion1(typeId).parseHeader(bytes)

  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {
    val typeBytes      = Bytes.ensureCapacity(Ints.toByteArray(typeId), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Longs.toByteArray(amount)
    val amountFill     = new Array[Byte](AmountLength - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes.arr, Bytes.concat(amountFill, amountBytes))

    val h = crypto.fastHash(data)
    Bytes.concat(h, h)
  }

  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    if (amount < 0) {
      Left(ValidationError.NegativeAmount(amount, "lto"))
    } else {
      val signature = ByteStr(generateSignature(recipient, amount, timestamp))
      Right(GenesisTransaction(1, networkByte, timestamp, recipient, amount, Proofs.fromSignature(signature)))
    }
  }
}
