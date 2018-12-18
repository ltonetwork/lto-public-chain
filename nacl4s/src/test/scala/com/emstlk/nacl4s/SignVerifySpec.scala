package com.emstlk.nacl4s

import com.emstlk.nacl4s.crypto.SigningKeyPair
import org.scalatest.{Assertion, FunSpec, Matchers}

import scala.util.Try

class SignVerifySpec extends FunSpec with Matchers {

  def sigValid(pub: Array[Byte], m: Array[Byte], sig: Array[Byte]): Boolean = Try(VerifyKey(pub).verify(m, sig)).fold(_ => false, _ => true)

  private def assert(keys: SigningKeyPair = SigningKeyPair())(length: Int, mutate: Boolean, result: Boolean): Assertion = {
    val keys      = SigningKeyPair()
    val pub       = keys.publicKey
    val priv      = keys.privateKey
    val bytes     = Array.fill[Byte](length)(30: Byte)
    val signature = SigningKey(priv).sign(bytes)
    if (mutate)
      signature(0) = 0
    val isSigCorrect = sigValid(pub, bytes, signature)
    isSigCorrect shouldBe result
  }

  describe("sign and verify bytes") {
    val seed = "seeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeed".getBytes().take(32) // seed must be 32 bytes
    def a    = assert(SigningKeyPair.apply(seed))(_, _, _)
    it("1 correct")(a(1, false, true))
    it("1 incorrect")(a(1, true, false))
    it("100 correct")(a(100, false, true))
    it("100 incorrect")(a(100, true, false))
    it("10000 correct")(a(10000, false, true))
    it("10000 incorrect")(a(10000, true, false))
  }
}
