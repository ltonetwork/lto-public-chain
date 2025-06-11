package com.ltonetwork.state.diffs

import com.ltonetwork.account.{KeyTypes, PublicKeyAccount}
import com.ltonetwork.state.{Blockchain, Diff, Portfolio}
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.certificate.CertificateTransaction
import com.ltonetwork.utils.Base58

import java.io.ByteArrayInputStream
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.interfaces.ECPublicKey

object CertificateTransactionDiff {

  private def extractPublicKey(certBytes: Array[Byte]): Either[ValidationError, PublicKeyAccount] = {
    try {
      val cf = CertificateFactory.getInstance("X.509")
      val cert = cf.generateCertificate(new ByteArrayInputStream(certBytes)).asInstanceOf[X509Certificate]
      val pubKey = cert.getPublicKey

      pubKey.getAlgorithm.toUpperCase match {
        case "EC" =>
          pubKey match {
            case ec: ECPublicKey =>
              val point = ec.getW
              val x = point.getAffineX
              val y = point.getAffineY

              val xBytes = x.toByteArray.dropWhile(_ == 0)
              val prefix: Byte = if (y.testBit(0)) 0x03.toByte else 0x02.toByte
              val compressed = Array(prefix) ++ xBytes.padTo(32, 0.toByte).takeRight(32)

              Right(PublicKeyAccount(KeyTypes.SECP256R1, compressed))

            case _ => Left(GenericError("EC key type not recognized"))
          }

        case "ED25519" =>
          val raw = pubKey.getEncoded.takeRight(32)
          Right(PublicKeyAccount(KeyTypes.ED25519, raw))

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
            Left(GenericError(s"Public key in certificate ${Base58.encode(certAccount.publicKey)} does not match sender's public key ${Base58.encode(tx.sender.publicKey)}"))
          else
            Right(Diff(
              height,
              tx,
              portfolios = Map(tx.sender.toAddress -> Portfolio.empty),
              certificate = Map(tx.sender.toAddress -> Some(cert))
            ))
        }

      case None =>
        Right(Diff(
          height,
          tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio.empty),
          certificate = Map(tx.sender.toAddress -> None)
        ))
    }
  }
}
