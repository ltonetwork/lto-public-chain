package com.ltonetwork.api.requests

import cats.implicits._
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json}

case class RegisterRequest(version: Option[Byte] = None,
                           timestamp: Option[Long] = None,
                           senderKeyType: Option[String] = None,
                           senderPublicKey: Option[String] = None,
                           fee: Long,
                           accounts: List[RegisterRequest.Account],
                           sponsorKeyType: Option[String] = None,
                           sponsorPublicKey: Option[String] = None,
                           signature: Option[ByteStr] = None,
                           proofs: Option[Proofs] = None)
    extends TxRequest.For[RegisterTransaction] {

  protected def sign(tx: RegisterTransaction, signer: PrivateKeyAccount): RegisterTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], time: Option[Time]): Either[ValidationError, RegisterTransaction] =
    for {
      validAccounts <- accounts.traverse(_.toAccount)
      validProofs   <- toProofs(signature, proofs)
      tx <- RegisterTransaction.create(
        version.getOrElse(RegisterTransaction.latestVersion),
        None,
        timestamp(time),
        sender,
        fee,
        validAccounts,
        sponsor,
        validProofs
      )
    } yield tx
}

object RegisterRequest {
  case class Account(keyType: String, publicKey: String) {
    def toAccount: Validation[PublicKeyAccount] = PublicKeyAccount.fromBase58String(keyType, publicKey)
  }

  implicit val jsonFormatKey: Format[RegisterRequest.Account] = Json.format[RegisterRequest.Account]

  implicit val jsonFormat: Format[RegisterRequest] = Format(
    Json.reads[RegisterRequest],
    Json.writes[RegisterRequest].transform((json: JsObject) => Json.obj("type" -> RegisterTransaction.typeId.toInt) ++ json)
  )
}
