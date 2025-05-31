package com.ltonetwork.api.requests

import cats.syntax.either._
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.certificate.CertificateTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Base64
import play.api.libs.json.{Format, JsObject, Json}

case class CertificateRequest(version: Option[Byte] = None,
                              timestamp: Option[Long] = None,
                              fee: Long,
                              sender: Option[String] = None,
                              senderKeyType: Option[String] = None,
                              senderPublicKey: Option[String] = None,
                              sponsor: Option[String] = None,
                              sponsorKeyType: Option[String] = None,
                              sponsorPublicKey: Option[String] = None,
                              certificate: Option[String], // base64 encoded
                              signature: Option[ByteStr] = None,
                              proofs: Option[Proofs] = None)
  extends TxRequest.For[CertificateTransaction] {

  protected def sign(tx: CertificateTransaction, signer: PrivateKeyAccount): CertificateTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, CertificateTransaction] = {
    val certBytesEither: Either[ValidationError, Array[Byte]] = certificate match {
      case Some(str) if str.nonEmpty =>
        Base64.decode(str).toEither.leftMap(ex => ValidationError.GenericError(s"Failed to decode certificate field ${ex.getMessage}"))
      case _ =>
        Right(Array.emptyByteArray)
    }

    for {
      certBytes <- certBytesEither
      tx <- CertificateTransaction.create(
        version.getOrElse(CertificateTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        certBytes,
        sponsor,
        proofs
      )
    } yield tx
  }
}

object CertificateRequest {
  implicit val jsonFormat: Format[CertificateRequest] = Format(
    Json.reads[CertificateRequest],
    Json.writes[CertificateRequest].transform((json: JsObject) =>
      Json.obj("type" -> CertificateTransaction.typeId.toInt) ++ json
    )
  )
}
