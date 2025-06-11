package com.ltonetwork.transaction.certificate

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import com.ltonetwork.utils.Base64
import monix.eval.Coeval
import play.api.libs.json._

import org.bouncycastle.cert.X509CertificateHolder

case class CertificateTransaction private (
                                            version: Byte,
                                            chainId: Byte,
                                            timestamp: Long,
                                            sender: PublicKeyAccount,
                                            fee: Long,
                                            certificate: Array[Byte],
                                            sponsor: Option[PublicKeyAccount],
                                            proofs: Proofs
                                          ) extends Transaction {

  override def builder: TransactionBuilder.For[CertificateTransaction] = CertificateTransaction
  private def serializer: TransactionSerializer.For[CertificateTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))

  val json: Coeval[JsObject] = Coeval.evalOnce {
    val certJson = if (certificate.isEmpty) {
      JsNull
    } else {
      val base64 = Base64.encode(certificate)
      val pem = s"""-----BEGIN CERTIFICATE-----\n$base64\n-----END CERTIFICATE-----"""
      JsString(pem)
    }

    jsonBase ++ Json.obj(
      "certificate" -> certJson
    )
  }
}

object CertificateTransaction extends TransactionBuilder.For[CertificateTransaction] {
  override val typeId: Byte = 24
  override val supportedVersions: Set[Byte] = Set(3)

  val MaxBytes: Int = 10 * 1024

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._

      val certIsValid = certificate.isEmpty || (try {
        new X509CertificateHolder(certificate)
        true
      } catch {
        case _: Throwable => false
      })

      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(certIsValid, (), ValidationError.GenericError("Invalid X.509 certificate (not DER-encoded?)")),
        Validated.condNel(certificate.length <= MaxBytes, (), ValidationError.TooBigArray)
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 3 => CertificateSerializerV3
    case _ => UnknownSerializer
  }

  def create(
              version: Byte,
              chainId: Option[Byte],
              timestamp: Long,
              sender: PublicKeyAccount,
              fee: Long,
              certificate: Array[Byte],
              sponsor: Option[PublicKeyAccount],
              proofs: Proofs
            ): Either[ValidationError, TransactionT] =
    CertificateTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, certificate, sponsor, proofs).validatedEither

  def signed(
              version: Byte,
              timestamp: Long,
              sender: PrivateKeyAccount,
              fee: Long,
              certificate: Array[Byte]
            ): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, certificate, None, Proofs.empty).signWith(sender)
}
