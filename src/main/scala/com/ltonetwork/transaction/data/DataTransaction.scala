package com.ltonetwork.transaction.data

import cats.implicits._
import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json._
import scala.util.Try

case class DataTransaction private (version: Byte,
                                    chainId: Byte,
                                    timestamp: Long,
                                    sender: PublicKeyAccount,
                                    fee: Long,
                                    data: List[DataEntry[_]],
                                    sponsor: Option[PublicKeyAccount],
                                    proofs: Proofs)
    extends Transaction {

  override def builder: TransactionBuilder.For[DataTransaction] = DataTransaction
  private def serializer: TransactionSerializer.For[DataTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  override val json: Coeval[JsObject] = Coeval.evalOnce(serializer.toJson(this))

  implicit val dataItemFormat: Format[DataEntry[_]] = DataEntry.Format
}

object DataTransaction extends TransactionBuilder.For[DataTransaction] {

  override val typeId: Byte                 = 12
  override val supportedVersions: Set[Byte] = Set(1)

  val MaxBytes: Int      = 150 * 1024
  val MaxEntryCount: Int = 100

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT =
    tx.copy(proofs = Proofs(crypto.sign(signer, tx.bodyBytes())))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._

      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(data.lengthCompare(MaxEntryCount) <= 0 && data.forall(_.valid), None, ValidationError.TooBigArray),
        Validated.condNel(!data.exists(_.key.isEmpty), None, ValidationError.GenericError("Empty key found")),
        Validated.condNel(data.map(_.key).distinct.lengthCompare(data.size) == 0, None, ValidationError.GenericError("Duplicate keys found")),
        Try { Validated.condNel(bytes().length <= MaxBytes, None, ValidationError.TooBigArray) }.getOrElse(None.validNel),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3, None, ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => DataSerializerV1
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             data: List[DataEntry[_]],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    DataTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, data, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             data: List[DataEntry[_]],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, data, None, Proofs.empty).signWith(signer)

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 data: List[DataEntry[_]]): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, data, sender)
}
