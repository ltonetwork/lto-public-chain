package com.ltonetwork.api.requests

import cats.implicits._
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.anchor.MappedAnchorTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class MappedAnchorRequest(version: Option[Byte] = None,
                               timestamp: Option[Long] = None,
                               fee: Long,
                               sender: Option[String] = None,
                               senderKeyType: Option[String] = None,
                               senderPublicKey: Option[String] = None,
                               sponsor: Option[String] = None,
                               sponsorKeyType: Option[String] = None,
                               sponsorPublicKey: Option[String] = None,
                               anchors: Map[String, String],
                               signature: Option[ByteStr] = None,
                               proofs: Option[Proofs] = None)
    extends TxRequest.For[MappedAnchorTransaction] {

  protected def sign(tx: MappedAnchorTransaction, signer: PrivateKeyAccount): MappedAnchorTransaction = tx.signWith(signer)

  private def parseAnchor(k: String, v: String): ValidationError.Validation[(ByteStr, ByteStr)] =
    for {
      kb <- parseBase58(k, "invalid anchor key", MappedAnchorTransaction.MaxAnchorStringSize)
      vb <- parseBase58(v, "invalid anchor hash", MappedAnchorTransaction.MaxAnchorStringSize)
    } yield (kb, vb)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, MappedAnchorTransaction] =
    for {
      validAnchors <- anchors.toList.traverse { case (k, v) => parseAnchor(k, v) }.map(_.toMap)
      tx <- MappedAnchorTransaction.create(
        version.getOrElse(MappedAnchorTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        validAnchors,
        sponsor,
        proofs
      )
    } yield tx
}

object MappedAnchorRequest {
  implicit val jsonFormat: Format[MappedAnchorRequest] = Format(
    Json.reads[MappedAnchorRequest],
    Json.writes[MappedAnchorRequest].transform((json: JsObject) => Json.obj("type" -> MappedAnchorTransaction.typeId.toInt) ++ json)
  )
}
