package com.ltonetwork.transaction.lease

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import com.ltonetwork.transaction.Transaction.{HardcodedV1, SigProofsSwitch}
import com.ltonetwork.transaction.TransactionParser.{HardcodedVersion1, MultipleVersions}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Either, Try}

case class LeaseTransaction private (version: Byte,
                                     chainId: Byte,
                                     timestamp: Long,
                                     sender: PublicKeyAccount,
                                     fee: Long,
                                     recipient: Address,
                                     amount: Long,
                                     sponsor: Option[PublicKeyAccount],
                                     proofs: Proofs)
    extends Transaction
    with HardcodedV1
    with SigProofsSwitch {

  override def builder: TransactionBuilder.For[LeaseTransaction]      = LeaseTransaction
  private def serializer: TransactionSerializer.For[LeaseTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj(
    "recipient" -> recipient.stringRepr,
    "amount"    -> amount
  ))
}

object LeaseTransaction extends TransactionBuilder.For[LeaseTransaction] {

  override val typeId: Byte                 = 8
  override def supportedVersions: Set[Byte] = Set(1, 2, 3)

  object Status {
    val Active   = "active"
    val Canceled = "canceled"
  }

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => LeaseSerializerV1
    case 2 => LeaseSerializerV2
    case 3 => LeaseSerializerV3
    case _ => UnknownSerializer
  }

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(amount > 0, None, ValidationError.NegativeAmount(amount, "lto")),
        Validated.condNel(Try(Math.addExact(amount, fee)).isSuccess, None, ValidationError.OverflowError),
        Validated.condNel(sender.stringRepr != recipient.stringRepr, None, ValidationError.ToSelf),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
        Validated.condNel(proofs.length <= 1 || version > 1, None, ValidationError.UnsupportedFeature(s"Multiple proofs not supported for tx v1")),
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
             recipient: Address,
             amount: Long,
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    LeaseTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, recipient, amount, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             amount: Long,
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, recipient, amount, sponsor, proofs).signWith(signer)

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 recipient: Address,
                 amount: Long): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, recipient, amount, None, Proofs.empty, sender)
}
