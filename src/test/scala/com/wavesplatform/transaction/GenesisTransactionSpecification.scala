package com.wavesplatform.transaction

import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.utils.Base58
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}

class GenesisTransactionSpecification extends PropSpec with PropertyChecks with Matchers {

  private val defaultRecipient = PublicKeyAccount(Array.fill(32)(0: Byte))

  property("GenesisTransaction Signature should be the same") {
    val balance   = 457L
    val timestamp = 2398762345L
    val signature = GenesisTransaction.generateSignature(defaultRecipient, balance, timestamp)

    val expected = "31kwJkQswbomM8XwKPrbuw1GoTy74c28kBGTFaT9obU3DEEeG2xjGNHxs9r6Usi7htFSGYfvyQjJ63cESMaUgWjY"
    val actual   = Base58.encode(signature)

    assert(actual == expected)
  }

  property("GenesisTransaction parse from Bytes should work fine") {

    val bytes = Base58.decode("5GoidXWjBfzuS9tYF43DP2KywVGXY7MXQv915BfoZU6uhu5UMDUU6ScmH2").get

    val actualTransaction = GenesisTransaction.parseBytes(bytes).get

    val balance             = 12345L
    val timestamp           = 1234567890L
    val expectedTransaction = GenesisTransaction.create(defaultRecipient, balance, timestamp).explicitGet()

    actualTransaction should equal(expectedTransaction)
  }

  property("GenesisTransaction serialize/deserialize roundtrip") {
    forAll(Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray), Gen.posNum[Long], Gen.posNum[Long]) {
      (recipientSeed: Array[Byte], time: Long, amount: Long) =>
        val recipient = PrivateKeyAccount(recipientSeed)
        val source    = GenesisTransaction.create(recipient, amount, time).explicitGet()
        val bytes     = source.bytes()
        val dest      = GenesisTransaction.parseBytes(bytes).get

        source should equal(dest)
    }
  }

}
