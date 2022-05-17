package com.ltonetwork.transaction.association

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json._

case class RevokeAssociationTransaction private (version: Byte,
                                                 chainId: Byte,
                                                 timestamp: Long,
                                                 sender: PublicKeyAccount,
                                                 fee: Long,
                                                 recipient: Address,
                                                 assocType: Int,
                                                 hash: Option[ByteStr],
                                                 sponsor: Option[PublicKeyAccount],
                                                 proofs: Proofs)
    extends AssociationTransaction {

  override def builder: TransactionBuilder.For[RevokeAssociationTransaction]      = RevokeAssociationTransaction
  private def serializer: TransactionSerializer.For[RevokeAssociationTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))

  val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase ++ (
      Json.obj(
        "associationType" -> assocType,
        "recipient"       -> recipient.stringRepr,
      ) ++
        hash.map(h => Json.obj("hash" -> h.base58)).getOrElse(Json.obj())
    ))
}

object RevokeAssociationTransaction extends TransactionBuilder.For[RevokeAssociationTransaction] {

  override def typeId: Byte                 = 17
  override def supportedVersions: Set[Byte] = Set(1, 3)

  val MaxHashLength: Int    = IssueAssociationTransaction.MaxHashLength
  val StringHashLength: Int = IssueAssociationTransaction.StringHashLength

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(version < 3 || !hash.exists(_.arr.length == 0), (), ValidationError.GenericError("Hash length must not be 0 bytes")),
        Validated.condNel(!hash.exists(_.arr.length > MaxHashLength),
                          (),
                          ValidationError.GenericError(s"Hash length must be <= ${MaxHashLength} bytes")),
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

  object SerializerV1 extends AssociationSerializerV1[RevokeAssociationTransaction] {
    protected val createTx = (version, chainId, timestamp, sender, fee, recipient, assocType, hash, proofs) =>
      create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, hash, None, proofs)
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => SerializerV1
    case 3 => RevokeAssociationSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             assocType: Int,
             hash: Option[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    RevokeAssociationTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, recipient, assocType, hash, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PrivateKeyAccount,
             fee: Long,
             recipient: Address,
             assocType: Int,
             hash: Option[ByteStr]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, recipient, assocType, hash, None, Proofs.empty).signWith(sender)
}
