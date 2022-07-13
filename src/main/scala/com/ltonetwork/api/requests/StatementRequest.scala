package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.statement.StatementTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class StatementRequest(version: Option[Byte] = None,
                            fee: Long,
                            timestamp: Option[Long] = None,
                            sender: Option[String] = None,
                            senderKeyType: Option[String] = None,
                            senderPublicKey: Option[String] = None,
                            sponsor: Option[String] = None,
                            sponsorKeyType: Option[String] = None,
                            sponsorPublicKey: Option[String] = None,
                            statementType: Long,
                            recipient: Option[String],
                            subject: Option[ByteStr] = None,
                            data: Option[List[DataEntry[_]]],
                            signature: Option[ByteStr] = None,
                            proofs: Option[Proofs] = None)
    extends TxRequest.For[StatementTransaction] {

  protected def sign(tx: StatementTransaction, signer: PrivateKeyAccount): StatementTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, StatementTransaction] =
    for {
      validRecipient <- recipient.noneIfEmpty.map(r => Address.fromString(r))
        .fold[Either[ValidationError, Option[Address]]](Right(None))(_.map(Some(_)))
      tx <- StatementTransaction.create(
        version.getOrElse(StatementTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        statementType,
        validRecipient,
        subject.noneIfEmpty,
        data.getOrElse(List.empty[DataEntry[_]]),
        sponsor,
        proofs
      )
    } yield tx
}

object StatementRequest {
  implicit val jsonFormat: Format[StatementRequest] = Format(
    Json.reads[StatementRequest],
    Json.writes[StatementRequest].transform((json: JsObject) => Json.obj("type" -> StatementTransaction.typeId.toInt) ++ json)
  )
}
