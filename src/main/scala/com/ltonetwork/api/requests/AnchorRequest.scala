package com.ltonetwork.api.requests

import cats.implicits._
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json}

case class AnchorRequest(version: Option[Byte] = None,
                         timestamp: Option[Long] = None,
                         fee: Long,
                         sender: Option[String] = None,
                         senderKeyType: Option[String] = None,
                         senderPublicKey: Option[String] = None,
                         sponsor: Option[String] = None,
                         sponsorKeyType: Option[String] = None,
                         sponsorPublicKey: Option[String] = None,
                         anchors: List[String],
                         signature: Option[ByteStr] = None,
                         proofs: Option[Proofs] = None)
    extends TxRequest.For[AnchorTransaction] {

  protected def sign(tx: AnchorTransaction, signer: PrivateKeyAccount): AnchorTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, AnchorTransaction] =
    for {
      validAnchors <- anchors.traverse(s => parseBase58(s, "invalid anchor", AnchorTransaction.MaxAnchorStringSize))
      tx <- AnchorTransaction.create(
        version.getOrElse(AnchorTransaction.latestVersion),
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

object AnchorRequest {
  implicit val jsonFormat: Format[AnchorRequest] = Format(
    Json.reads[AnchorRequest],
    Json.writes[AnchorRequest].transform((json: JsObject) => Json.obj("type" -> AnchorTransaction.typeId.toInt) ++ json)
  )
}
