package com.ltonetwork.transaction.genesis

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state.{ByteStr, _}
import com.ltonetwork.transaction.TransactionBuilders._
import com.ltonetwork.transaction.{Proofs, SigProofsSwitch, Transaction, TransactionBuilder, TransactionSerializer, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.JsObject

case class GenesisTransaction private (recipient: Address,
                                      amount: Long,
                                      timestamp: Long,
                                      proofs: Proofs
  ) extends Transaction with SigProofsSwitch {

  override val fee                        = 0
  override val id: Coeval[ByteStr]        = Coeval.evalOnce(signature)

  override def version: Byte = 1
  override def sender: PublicKeyAccount = PublicKeyAccount.Dummy
  override def sponsor: Option[PublicKeyAccount] = None

  override val builder: TransactionBuilder.For[GenesisTransaction] = GenesisTransaction
  private val serializer: TransactionSerializer.For[GenesisTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = serializer.bodyBytes(this)
  override val json: Coeval[JsObject] = serializer.toJson(this)

  override protected val prefixByte: Coeval[Array[Byte]] = Coeval.evalOnce(Array[Byte]())
  override protected val footerBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array[Byte]())
}

object GenesisTransaction extends TransactionBuilder.For[GenesisTransaction] {

  override val typeId: Byte = 1
  override val supportedVersions: Set[Byte] = Set(1)

  val RECIPIENT_LENGTH: Int = Address.AddressLength
  val BASE_LENGTH: Int      = TimestampLength + RECIPIENT_LENGTH + AmountLength

  override implicit def sign(tx: GenesisTransaction, signer: PrivateKeyAccount): GenesisTransaction =
    throw new Exception("Genesis transactions have a generated signature")

  override def serializer(version: Byte): TransactionSerializer.For[GenesisTransaction] = GenesisSerializer

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
      val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))
      Right(GenesisTransaction(recipient, amount, timestamp, Proofs.fromSignature(signature)))
    }
  }
}
