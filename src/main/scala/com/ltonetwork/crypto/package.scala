package com.ltonetwork

import com.ltonetwork.account.{KeyType, KeyTypes, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.seasalt.sign.{ECDSA, Ed25519}
import com.ltonetwork.seasalt.hash.{Blake2b256, SHA256}

package object crypto {
  val signatureLength: Int = 64 //Curve25519
  val keyLength: Int       = 32 //Curve25519
  val digestLength: Int    = 32
  val secp256k1: ECDSA = new ECDSA("secp256k1")
  val secp256r1: ECDSA = new ECDSA("secp256r1")
  val ed25519: Ed25519 = new Ed25519()

  def fastHash(m: Array[Byte]): Array[Byte]   = Blake2b256.hash(m).getBytes
  def fastHash(s: String): Array[Byte]        = fastHash(s.getBytes())
  def secureHash(m: Array[Byte]): Array[Byte] = SHA256.hash(Blake2b256.hash(m)).getBytes
  def secureHash(s: String): Array[Byte]      = secureHash(s.getBytes())

  def sign(account: PrivateKeyAccount, message: Array[Byte]): Array[Byte] =
    sign(account.privateKey, account.keyType, message)

  def sign(privateKey: Array[Byte], message: Array[Byte]): Array[Byte] =
    sign(privateKey, KeyTypes.ED25519, message)

  def sign(privateKey: Array[Byte], keyType: KeyType, message: Array[Byte]): Array[Byte] = keyType match {
    case KeyTypes.ED25519 =>
      ed25519.signDetached(message, privateKey).getBytes
    case KeyTypes.SECP256K1 =>
      secp256k1.signDetached(message, privateKey).getBytes
    case KeyTypes.SECP256R1 =>
      secp256r1.signDetached(message, privateKey).getBytes
    case _ =>
      throw new IllegalArgumentException("Unknown key type")
  }

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    verify(signature, message, publicKey, KeyTypes.ED25519)

  def verify(signature: Array[Byte], message: Array[Byte], account: PublicKeyAccount): Boolean =
    verify(signature, message, account.publicKey, account.keyType)

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte], keyType: KeyType): Boolean = keyType match {
    case KeyTypes.ED25519 =>
      ed25519.verify(message, signature, publicKey)
    case KeyTypes.SECP256K1 =>
      secp256k1.verify(message, signature, publicKey)
    case KeyTypes.SECP256R1 =>
      secp256r1.verify(message, signature, publicKey)
    case _ =>
      throw new IllegalArgumentException("Unknown key type")
  }

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) =
    createKeyPair(seed, KeyTypes.ED25519)

  def createKeyPair(seed: Array[Byte], keyType: KeyType): (Array[Byte], Array[Byte]) = keyType match {
    case KeyTypes.ED25519 =>
      val kp = ed25519.keyPairFromSeed(seed)
      (kp.getPrivateKey.getBytes, kp.getPublicKey.getBytes)
    case KeyTypes.SECP256K1 =>
      val kp = secp256k1.keyPairFromSeed(seed)
      (kp.getPrivateKey.getBytes, kp.getPublicKey.getBytes)
    case KeyTypes.SECP256R1 =>
      val kp = secp256r1.keyPairFromSeed(seed)
      (kp.getPrivateKey.getBytes, kp.getPublicKey.getBytes)
    case _ =>
      throw new IllegalArgumentException("Unknown key type")
  }
}
