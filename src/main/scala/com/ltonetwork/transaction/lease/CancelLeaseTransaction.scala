package com.ltonetwork.transaction.lease

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.Transaction.{HardcodedV1, SigProofsSwitch}
import com.ltonetwork.transaction.TransactionParser.{HardcodedVersion1, MultipleVersions}
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class CancelLeaseTransaction private (version: Byte,
                                           chainId: Byte,
                                           timestamp: Long,
                                           sender: PublicKeyAccount,
                                           fee: Long,
                                           leaseId: ByteStr,
                                           sponsor: Option[PublicKeyAccount],
                                           proofs: Proofs,
                                           effectiveSponsor: Option[Address] = None)
    extends Transaction
    with HardcodedV1
    with SigProofsSwitch {

  override def builder: TransactionBuilder.For[CancelLeaseTransaction]      = CancelLeaseTransaction
  private def serializer: TransactionSerializer.For[CancelLeaseTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase ++ Json.obj(
      "version"   -> version,
      "fee"       -> fee,
      "timestamp" -> timestamp,
      "leaseId"   -> leaseId.base58
    ))

  override def withEffectiveSponsor(effectiveSponsor: Option[Address]): CancelLeaseTransaction =
    this.copy(effectiveSponsor = effectiveSponsor)
}

object CancelLeaseTransaction extends TransactionBuilder.For[CancelLeaseTransaction] {
  override val typeId: Byte                 = 9
  override def supportedVersions: Set[Byte] = Set(1, 2, 3)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => CancelLeaseSerializerV1
    case 2 => CancelLeaseSerializerV2
    case 3 => CancelLeaseSerializerV3
    case _ => UnknownSerializer
  }

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(leaseId.arr.length == crypto.digestLength, (), ValidationError.GenericError("Lease transaction id is invalid")),
        Validated.condNel(fee > 0, (), ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          (),
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
        Validated.condNel(proofs.length <= 1 || version > 1, (), ValidationError.UnsupportedFeature(s"Multiple proofs not supported for tx v1")),
        Validated.condNel(sender.keyType == ED25519 || version >= 3,
                          (),
                          ValidationError.UnsupportedFeature(s"Sender key type ${sender.keyType} not supported for tx v$version")),
      )
    }
  }

  override def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] =
    if (bytes(0) != 0) HardcodedVersion1(typeId).parseHeader(bytes)
    else MultipleVersions(typeId, supportedVersions).parseHeader(bytes)

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             leaseId: ByteStr,
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    CancelLeaseTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, leaseId, sponsor, proofs).validatedEither

  def signed(version: Byte, timestamp: Long, sender: PrivateKeyAccount, fee: Long, leaseId: ByteStr): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, leaseId, None, Proofs.empty).signWith(sender)
}
