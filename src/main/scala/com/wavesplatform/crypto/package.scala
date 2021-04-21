package com.wavesplatform

import com.emstlk.nacl4s.crypto.SigningKeyPair
import com.emstlk.nacl4s.{SigningKey, VerifyKey}
import java.security.{KeyFactory, Signature}
import java.security.spec.X509EncodedKeySpec
import com.wavesplatform.account.PrivateKeyAccount
import scorex.crypto.hash.{Blake2b256, Sha256}
import scala.util.Try

package object crypto {
  val SignatureLength: Int = 64

  val DigestSize: Int = 32

  def fastHash(m: Array[Byte]): Array[Byte] = Blake2b256.hash(m)

  def fastHash(s: String): Array[Byte] = fastHash(s.getBytes())

  def secureHash(m: Array[Byte]): Array[Byte] = Sha256.hash(Blake2b256.hash(m))

  def secureHash(s: String): Array[Byte] = secureHash(s.getBytes())

  def sign(account: PrivateKeyAccount, message: Array[Byte]): Array[Byte] =
    sign(account.privateKey, message)

  def sign(privateKeyBytes: Array[Byte], message: Array[Byte]): Array[Byte] =
    SigningKey(privateKeyBytes).sign(message)

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    Try(VerifyKey(publicKey).verify(message, signature)).fold(_ => false, _ => true)

  def verifyEcdsa(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = {
    val ecdsaVerify = Signature.getInstance("SHA256withECDSA")
    val keyFactory = KeyFactory.getInstance("EC")
    val publicKeySpec = new X509EncodedKeySpec(signature)
    val publicKey = keyFactory.generatePublic(publicKeySpec)

    ecdsaVerify.initVerify(publicKey)
    ecdsaVerify.update(message)
    ecdsaVerify.verify(signature)
  }

  def verifyAny(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    verify(signature, message, publicKey) || verifyEcdsa(signature, message, publicKey)

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val kp = SigningKeyPair(Sha256.hash(seed))
    (kp.privateKey, kp.publicKey)
  }
}
