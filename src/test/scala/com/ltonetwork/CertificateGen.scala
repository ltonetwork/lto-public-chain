package com.ltonetwork

import java.math.BigInteger
import java.security._
import java.util.Date
import org.bouncycastle.asn1.x500.X500Name
import org.bouncycastle.cert.jcajce.{JcaX509CertificateConverter, JcaX509v3CertificateBuilder}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.{ECPrivateKeySpec, ECPublicKeySpec}
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder
import org.scalacheck.Gen
import com.ltonetwork.account.PrivateKeyAccount
import org.bouncycastle.jce.ECNamedCurveTable

trait CertificateGen {
  Security.addProvider(new BouncyCastleProvider())

  def validCertificateFor(sender: PrivateKeyAccount): Gen[Array[Byte]] = Gen.delay {
    val keyFactory = KeyFactory.getInstance("EC", "BC")
    val ecSpec = ECNamedCurveTable.getParameterSpec("secp256r1")

    val privateKeySpec = new ECPrivateKeySpec(new BigInteger(1, sender.privateKey), ecSpec)
    val publicKeySpec = new ECPublicKeySpec(ecSpec.getG.multiply(new BigInteger(1, sender.privateKey)), ecSpec)

    val privateKey = keyFactory.generatePrivate(privateKeySpec)
    val publicKey = keyFactory.generatePublic(publicKeySpec)

    val now = System.currentTimeMillis()
    val startDate = new Date(now - 10000)
    val endDate = new Date(now + 365L * 24 * 60 * 60 * 1000)

    val dn = new X500Name("CN=LTO Test")
    val serial = BigInteger.valueOf(now)

    val certBuilder = new JcaX509v3CertificateBuilder(
      dn, serial, startDate, endDate, dn, publicKey
    )

    val signer = new JcaContentSignerBuilder("SHA256withECDSA")
      .setProvider("BC")
      .build(privateKey)

    val cert = new JcaX509CertificateConverter()
      .setProvider("BC")
      .getCertificate(certBuilder.build(signer))

    cert.getEncoded
  }
}
