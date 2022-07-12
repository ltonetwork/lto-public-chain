package com.ltonetwork.transaction.statement

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json.{Json, _}

case class StatementTransaction private(version: Byte,
                                        chainId: Byte,
                                        timestamp: Long,
                                        sender: PublicKeyAccount,
                                        fee: Long,
                                        statementType: Long,
                                        recipient: Option[Address],
                                        subject: Option[ByteStr],
                                        data: List[DataEntry[_]],
                                        sponsor: Option[PublicKeyAccount],
                                        proofs: Proofs)
  extends Transaction {

  override def builder: TransactionBuilder.For[StatementTransaction]      = StatementTransaction
  private def serializer: TransactionSerializer.For[StatementTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))

  val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase ++
      Json.obj("statementType" -> statementType) ++
      recipient.fold(Json.obj())(v => Json.obj("recipient" -> v)) ++
      subject.fold(Json.obj())(v => Json.obj("subject" -> v)) ++
      (if (data.nonEmpty) Json.obj("data" -> data) else Json.obj())
  )
}

object StatementTransaction extends TransactionBuilder.For[StatementTransaction] {

  override val typeId: Byte                 = 20
  override val supportedVersions: Set[Byte] = Set(3)

  val MaxSubjectLength: Int  = 256
  val MaxBytes: Int          = 150 * 1024
  val MaxEntryCount: Int     = 100

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(!subject.exists(_.length > MaxSubjectLength), (), ValidationError.GenericError(s"Subject length must be <= $MaxSubjectLength bytes")),
        Validated.condNel(data.lengthCompare(MaxEntryCount) <= 0 && data.forall(_.valid), (), ValidationError.TooBigArray),
        Validated.condNel(!data.exists(_.key.isEmpty), (), ValidationError.GenericError("Empty key found in data")),
        Validated.condNel(data.map(_.key).distinct.lengthCompare(data.size) == 0, (), ValidationError.GenericError("Duplicate keys found in data")),
        Validated.condNel(data.flatMap(_.toBytes).toArray.length <= MaxBytes, (), ValidationError.TooBigArray),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 3 => StatementSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             statementType: Long,
             recipient: Option[Address],
             subject: Option[ByteStr],
             data: List[DataEntry[_]],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    StatementTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, statementType, recipient, subject, data, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PrivateKeyAccount,
             fee: Long,
             statementType: Long,
             recipient: Option[Address],
             subject: Option[ByteStr],
             data: List[DataEntry[_]]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, statementType, recipient, subject, data, None, Proofs.empty).signWith(sender)
}
