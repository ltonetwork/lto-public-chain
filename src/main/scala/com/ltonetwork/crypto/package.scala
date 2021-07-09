package com.ltonetwork

import com.emstlk.nacl4s.crypto.SigningKeyPair
import com.emstlk.nacl4s.{SigningKey, VerifyKey}
import com.ltonetwork.account.{KeyType, KeyTypes, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.hash.{Blake2b256, Sha256}
import scorex.crypto.signatures.Curve25519
import com.ltonetwork.seasalt.sign.{ECDSA, Ed25519, Signer}

import scala.util.Try

package object crypto {
  val SignatureLength: Int = Curve25519.SignatureLength
  val KeyLength: Int       = Curve25519.KeyLength
  val DigestLength: Int    = 32

  def fastHash(m: Array[Byte]): Array[Byte] = Blake2b256.hash(m)
  def fastHash(s: String): Array[Byte] = fastHash(s.getBytes())
  def secureHash(m: Array[Byte]): Array[Byte] = Sha256.hash(Blake2b256.hash(m))
  def secureHash(s: String): Array[Byte] = secureHash(s.getBytes())

  def sign(account: PrivateKeyAccount, message: Array[Byte]): Array[Byte] =
    sign(account.privateKey, account.keyType, message)

  def sign(privateKeyBytes: Array[Byte], message: Array[Byte]): Array[Byte] =
    sign(privateKeyBytes, KeyTypes.ED25519, message)

  def sign(privateKeyBytes: Array[Byte], keyType: KeyType, message: Array[Byte]): Array[Byte] = keyType match {
    case KeyTypes.ED25519 =>
      new Ed25519().signDetached(message, privateKeyBytes).getBytes
    case KeyTypes.SECP256K1 =>
      new ECDSA("secp256k1").signDetached(message, privateKeyBytes).getBytes
    case KeyTypes.SECP256R1 =>
      new ECDSA("secp256r1").signDetached(message, privateKeyBytes).getBytes
  }

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    Try(VerifyKey(publicKey).verify(message, signature)).fold(_ => false, _ => true)

  def verify(signature: Array[Byte], message: Array[Byte], account: PublicKeyAccount): Boolean =
    verify(signature, message, account.publicKey)

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val kp = SigningKeyPair(Sha256.hash(seed))
    (kp.privateKey, kp.publicKey)
  }
}
