package com.ltonetwork.transaction.smart

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account._
import com.ltonetwork.crypto
import com.ltonetwork.serialization.Deser
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.smart.script.{Script, ScriptReader}
import monix.eval.Coeval
import play.api.libs.json.JsObject

case class SetScriptTransaction private (version: Byte,
                                         chainId: Byte,
                                         timestamp: Long,
                                         sender: PublicKeyAccount,
                                         fee: Long,
                                         script: Option[Script],
                                         sponsor: Option[PublicKeyAccount],
                                         proofs: Proofs)
    extends Transaction {

  override val builder: TransactionBuilder.For[SetScriptTransaction] = SetScriptTransaction
  private val serializer: TransactionSerializer.For[SetScriptTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = serializer.bodyBytes(this)
  override val json: Coeval[JsObject] = serializer.toJson(this)
}

object SetScriptTransaction extends TransactionBuilder.For[SetScriptTransaction] {

  override val typeId: Byte                 = 13
  override val supportedVersions: Set[Byte] = Set(1)

  def parseScript(bytes: Array[Byte], start: Int): (Either[ValidationError.ScriptParseError, Option[Script]], Int) = {
    val (scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]], scriptEnd) =
      Deser.parseOption(bytes, start)(ScriptReader.fromBytes)

    val scriptOpt = scriptOptEi match {
      case None => Right(None)
      case Some(Right(sc)) => Right(Some(sc))
      case Some(Left(err)) => Left(err)
    }

    (scriptOpt, scriptEnd)
  }

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT =
    tx.copy(proofs = Proofs(crypto.sign(signer, tx.bodyBytes())))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3, None, ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => SetTransactionSerializerV1
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

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             script: Option[Script],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, script, None, Proofs.empty).signWith(signer)

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 script: Option[Script]): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, script, sender)
}
