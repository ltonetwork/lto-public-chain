package com.ltonetwork.crypto

import com.ltonetwork.crypto
import com.ltonetwork.account.KeyTypes._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.ltonetwork.account.PrivateKeyAccount

class SigningFunctionsSpecification extends PropSpec with PropertyChecks with Matchers {

  property("signed message should be verifiable with appropriate ED25519 public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte], message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val acc      = PrivateKeyAccount(seed1)
        val sig      = crypto.sign(acc, message1)
        val rightKey = acc.publicKey
        crypto.verify(sig, message1, rightKey) should be(true)

        val wrongKey = PrivateKeyAccount(seed2).publicKey
        crypto.verify(sig, message1, wrongKey) shouldNot be(true)

        crypto.verify(sig, message2, rightKey) shouldNot be(true)
      }
    }
  }

  property("signed message should be verifiable with appropriate SECP256K1 public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte], message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val acc      = PrivateKeyAccount(seed1, SECP256K1)
        val sig      = crypto.sign(acc, message1)
        val rightKey = acc.publicKey
        crypto.verify(sig, message1, rightKey, SECP256K1) should be(true)

        val wrongKey = PrivateKeyAccount(seed2).publicKey
        crypto.verify(sig, message1, wrongKey, SECP256K1) shouldNot be(true)

        crypto.verify(sig, message2, rightKey, SECP256K1) shouldNot be(true)
      }
    }
  }

  property("signed message should be verifiable with appropriate SECP256R1 public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte], message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val acc      = PrivateKeyAccount(seed1, SECP256R1)
        val sig      = crypto.sign(acc, message1)
        val rightKey = acc.publicKey
        crypto.verify(sig, message1, rightKey, SECP256R1) should be(true)

        val wrongKey = PrivateKeyAccount(seed2).publicKey
        crypto.verify(sig, message1, wrongKey, SECP256R1) shouldNot be(true)

        crypto.verify(sig, message2, rightKey, SECP256R1) shouldNot be(true)
      }
    }
  }
}
