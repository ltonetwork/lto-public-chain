package com.ltonetwork.transaction.register

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json._

case class RegisterTransaction private (version: Byte,
                                      chainId: Byte,
                                      timestamp: Long,
                                      sender: PublicKeyAccount,
                                      fee: Long,
                                      keys: List[PublicKeyAccount],
                                      sponsor: Option[PublicKeyAccount],
                                      proofs: Proofs)
  extends Transaction {

  override def builder: TransactionBuilder.For[RegisterTransaction]      = RegisterTransaction
  private def serializer: TransactionSerializer.For[RegisterTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj("keys" -> Json.toJson(
    keys.map(key => Json.obj("keyType" -> key.keyType.reference, "publicKey" -> Base58.encode(key.publicKey)))
  )))
}

object RegisterTransaction extends TransactionBuilder.For[RegisterTransaction] {

  override val typeId: Byte                 = 17
  override val supportedVersions: Set[Byte] = Set(3)

  val MaxEntryCount: Int = 100

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(keys.lengthCompare(MaxEntryCount) <= 0, (), ValidationError.TooBigArray),
        Validated.condNel(keys.forall(k => k.publicKey.length == k.keyType.length),
          (),
          ValidationError.GenericError(s"Invalid key length")),
        Validated.condNel(keys.map(_.publicKey).distinct.lengthCompare(keys.size) == 0,
          (),
          ValidationError.GenericError("Duplicate key in one tx found")),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee())
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 3 => RegisterSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             keys: List[PublicKeyAccount],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    RegisterTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, keys, sponsor, proofs).validatedEither

  def signed(version: Byte, timestamp: Long, sender: PrivateKeyAccount, fee: Long, keys: List[PublicKeyAccount]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, keys, None, Proofs.empty).signWith(sender)
}
