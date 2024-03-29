package com.ltonetwork.transaction.data

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
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

  override def builder: TransactionBuilder.For[DataTransaction]      = DataTransaction
  private def serializer: TransactionSerializer.For[DataTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj("data" -> Json.toJson(data)))

  implicit val dataItemFormat: Format[DataEntry[_]] = DataEntry.Format
}

object DataTransaction extends TransactionBuilder.For[DataTransaction] {

  override val typeId: Byte                 = 12
  override val supportedVersions: Set[Byte] = Set(1, 3)

  val MaxBytes: Int      = 10 * 1024
  val MaxEntryCount: Int = 100

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(data.lengthCompare(MaxEntryCount) <= 0 && data.forall(_.valid), (), ValidationError.TooBigArray),
        Validated.condNel(!data.exists(_.key.isEmpty), (), ValidationError.GenericError("Empty key found")),
        Validated.condNel(data.map(_.key).distinct.lengthCompare(data.size) == 0, (), ValidationError.GenericError("Duplicate keys found")),
        Validated.condNel(data.flatMap(_.toBytes).toArray.length <= MaxBytes, (), ValidationError.TooBigArray),
        Validated.condNel(fee > 0, (), ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          (),
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
        Validated.condNel(sender.keyType == ED25519 || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sender key type ${sender.keyType} not supported for tx v$version"))
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => DataSerializerV1
    case 3 => DataSerializerV3
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

  def signed(version: Byte, timestamp: Long, sender: PrivateKeyAccount, fee: Long, data: List[DataEntry[_]]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, data, None, Proofs.empty).signWith(sender)
}
