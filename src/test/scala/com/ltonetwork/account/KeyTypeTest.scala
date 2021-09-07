package com.ltonetwork.account

import org.scalatest.{FlatSpec, Matchers}
import com.ltonetwork.account.KeyTypes._
import org.scalatest.TryValues._

import scala.util.{Failure, Success}

class KeyTypeTest extends FlatSpec with Matchers {
  "KeyTypes" should "resolve by id" in {
    keyType(1: Byte) shouldBe Success(ED25519)
    keyType(2: Byte) shouldBe Success(SECP256K1)
    keyType(3: Byte) shouldBe Success(SECP256R1)

    keyType(32: Byte).failure.exception should have message "Unknown key type id 32"
  }

  "KeyTypes" should "resolve by reference" in {
    keyType("ed25519") shouldBe Success(ED25519)
    keyType("secp256k1") shouldBe Success(SECP256K1)
    keyType("secp256r1") shouldBe Success(SECP256R1)

    keyType("ED25519") shouldBe Success(ED25519)

    keyType("foo").failure.exception should have message "Unknown key type 'foo'"
  }
}
