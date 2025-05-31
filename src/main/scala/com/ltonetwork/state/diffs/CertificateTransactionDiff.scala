package com.ltonetwork.state.diffs

import com.ltonetwork.account.{PublicKeyAccount, KeyTypes}
import com.ltonetwork.state.{Blockchain, Diff, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.certificate.CertificateTransaction

import java.io.ByteArrayInputStream
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.interfaces.ECPublicKey
import java.security.PublicKey

object CertificateTransactionDiff {

  private def extractPublicKey(certBytes: Array[Byte]): Either[ValidationError, PublicKeyAccount] = {
    try {
      val cf: CertificateFactory = CertificateFactory.getInstance("X.509")
      val cert = cf.generateCertificate(new ByteArrayInputStream(certBytes)).asInstanceOf[X509Certificate]
      val pubKey: PublicKey = cert.getPublicKey

      pubKey.getAlgorithm.toUpperCase match {
        case "ED25519" =>
          val raw = pubKey.getEncoded.takeRight(32)
          Right(PublicKeyAccount(KeyTypes.ED25519, raw))

        case "EC" =>
          pubKey match {
            case ec: ECPublicKey =>
              val fieldSize = ec.getParams.getCurve.getField.getFieldSize
              if (fieldSize == 256) {
                val raw = pubKey.getEncoded.takeRight(65) // 0x04 + X(32) + Y(32)
                Right(PublicKeyAccount(KeyTypes.SECP256R1, raw))
              } else {
                Left(GenericError("Unsupported EC curve"))
              }
            case _ => Left(GenericError("EC key type not recognized"))
          }

        case other =>
          Left(GenericError(s"Unsupported key algorithm: $other"))
      }
    } catch {
      case e: Exception =>
        Left(GenericError(s"Invalid certificate format: ${e.getMessage}"))
    }
  }

  def apply(blockchain: Blockchain, height: Int)(tx: CertificateTransaction): Either[ValidationError, Diff] = {
    val certOpt: Option[Array[Byte]] = Option(tx.certificate).filter(_.nonEmpty)

    certOpt match {
      case Some(cert) =>
        extractPublicKey(cert).flatMap { certAccount =>
          if (tx.sender.keyType != certAccount.keyType)
            Left(GenericError("Key type in certificate does not match sender's key type"))
          else if (!tx.sender.publicKey.sameElements(certAccount.publicKey))
            Left(GenericError("Public key in certificate does not match sender's public key"))
          else
            Right(Diff(
              height,
              tx,
              portfolios = Map(tx.sender.toAddress -> Portfolio.empty),
              certificate = Map(tx.sender.toAddress -> Some(cert))
            ))
        }

      case None =>
        // Clear certificate
        Right(Diff(
          height,
          tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio.empty),
          certificate = Map(tx.sender.toAddress -> None)
        ))
    }
  }
}
