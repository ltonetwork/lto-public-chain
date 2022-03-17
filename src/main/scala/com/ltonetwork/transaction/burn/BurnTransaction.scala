package com.ltonetwork.transaction.burn

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.Try

case class BurnTransaction private(version: Byte,
                                   chainId: Byte,
                                   timestamp: Long,
                                   sender: PublicKeyAccount,
                                   fee: Long,
                                   amount: Long,
                                   sponsor: Option[PublicKeyAccount],
                                   proofs: Proofs)
    extends Transaction {

  override def builder: TransactionBuilder.For[BurnTransaction]      = BurnTransaction
  private def serializer: TransactionSerializer.For[BurnTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject] = Coeval.evalOnce(jsonBase ++ Json.obj("amount" -> amount))
}


object BurnTransaction extends TransactionBuilder.For[BurnTransaction] {

  override val typeId: Byte                 = 21
  override val supportedVersions: Set[Byte] = Set(3)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(Try(Math.addExact(amount, fee)).isSuccess, None, ValidationError.OverflowError),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(amount > 0, (), ValidationError.NegativeAmount(amount, "lto")),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 3 => BurnSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             amount: Long,
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    BurnTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, amount, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PrivateKeyAccount,
             fee: Long,
             amount: Long): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, amount, None, Proofs.empty).signWith(sender)
}
