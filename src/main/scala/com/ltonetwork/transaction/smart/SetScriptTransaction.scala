package com.ltonetwork.transaction.smart

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.account._
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.smart.script.{Script, ScriptReader}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import java.nio.ByteBuffer

case class SetScriptTransaction private (version: Byte,
                                         chainId: Byte,
                                         timestamp: Long,
                                         sender: PublicKeyAccount,
                                         fee: Long,
                                         script: Option[Script],
                                         sponsor: Option[PublicKeyAccount],
                                         proofs: Proofs,
                                         effectiveSponsor: Option[Address] = None)
    extends Transaction {

  override def builder: TransactionBuilder.For[SetScriptTransaction]      = SetScriptTransaction
  private def serializer: TransactionSerializer.For[SetScriptTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj("script" -> script.map(_.bytes().base64)))

  override def withEffectiveSponsor(effectiveSponsor: Option[Address]): SetScriptTransaction =
    this.copy(effectiveSponsor = effectiveSponsor)
}

object SetScriptTransaction extends TransactionBuilder.For[SetScriptTransaction] {

  override val typeId: Byte                 = 13
  override val supportedVersions: Set[Byte] = Set(1, 3)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
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
    case 1 => SetScriptSerializerV1
    case 3 => SetScriptSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             script: Option[Script],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    SetScriptTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, script, sponsor, proofs).validatedEither

  def signed(version: Byte, timestamp: Long, sender: PrivateKeyAccount, fee: Long, script: Option[Script]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, script, None, Proofs.empty).signWith(sender)
}
