package com.ltonetwork

import java.math.BigInteger
import java.security._
import java.security.cert.X509Certificate
import java.util.Date

import org.bouncycastle.asn1.x500.X500Name
import org.bouncycastle.cert.jcajce.{JcaX509CertificateConverter, JcaX509v3CertificateBuilder}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.ECNamedCurveGenParameterSpec
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder

import org.scalacheck.Gen
import com.ltonetwork.account.{KeyTypes, PublicKeyAccount}

trait CertificateGen {
  Security.addProvider(new BouncyCastleProvider())

  def validCertificateFor(sender: PublicKeyAccount): Gen[Array[Byte]] = {
    require(sender.keyType == KeyTypes.SECP256R1, "Only secp256r1 is supported in this generator")

    Gen.delay {
      val keyPairGen = KeyPairGenerator.getInstance("EC", "BC")
      keyPairGen.initialize(new ECNamedCurveGenParameterSpec("secp256r1"), new SecureRandom())
      val keyPair = keyPairGen.generateKeyPair()

      val now = System.currentTimeMillis()
      val startDate = new Date(now - 10000)
      val endDate = new Date(now + 365L * 24 * 60 * 60 * 1000)

      val dn = new X500Name("CN=LTO Test")
      val serial = BigInteger.valueOf(now)

      val certBuilder = new JcaX509v3CertificateBuilder(
        dn, serial, startDate, endDate, dn, keyPair.getPublic
      )

      val signer = new JcaContentSignerBuilder("SHA256withECDSA").setProvider("BC").build(keyPair.getPrivate)
      val cert: X509Certificate = new JcaX509CertificateConverter().setProvider("BC").getCertificate(certBuilder.build(signer))

      cert.getEncoded
    }
  }
}
