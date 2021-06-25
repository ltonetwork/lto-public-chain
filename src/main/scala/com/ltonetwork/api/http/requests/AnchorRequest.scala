package com.ltonetwork.api.http.requests

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, Json}

case class AnchorRequest(version: Option[Byte],
                         sender: Option[String],
                         senderPublicKey: Option[String],
                         anchors: List[String],
                         fee: Long,
                         timestamp: Option[Long] = None,
                         signature: Option[ByteStr] = None,
                         proofs: Option[Proofs] = None,
    ) extends TxRequest {

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, AnchorTransaction] =
    for {
      validProofs  <- toProofs(signature, proofs)
      validAnchors <- anchors.traverse(s => parseBase58(s, "invalid anchor", AnchorTransaction.MaxAnchorStringSize))
      tx <- AnchorTransaction.create(
        version.getOrElse(AnchorTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validAnchors,
        None,
        validProofs
      )
    } yield tx
}

object AnchorRequest {
  implicit val jsonFormat: Format[AnchorRequest] = Json.format[AnchorRequest]
}
