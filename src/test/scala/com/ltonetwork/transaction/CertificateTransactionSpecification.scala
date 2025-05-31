package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.{KeyType, KeyTypes, PublicKeyAccount}
import com.ltonetwork.api.requests.{CertificateRequest, LeaseRequest}
import com.ltonetwork.transaction.certificate.CertificateTransaction
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.utils.{Base58, Base64}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.Assertion
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.Json

class CertificateTransactionSpecification
  extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with TableDrivenPropertyChecks
    with Matchers
    with TransactionGen {

  private def checkSerialization(tx: CertificateTransaction): Assertion = {
    val tryParse = CertificateTransaction.parseBytes(tx.bytes())
    tryParse should be a 'success

    val parsed = tryParse.get
    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee
    parsed.sponsor shouldEqual tx.sponsor
    parsed.certificate shouldEqual tx.certificate
    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(certificateTransactionGen)(checkSerialization)
  }

  property("from TransactionBuilder") {
    forAll(certificateTransactionGen) { tx: CertificateTransaction =>
      val recovered = TransactionBuilders.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {
    forAll(certificateTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[CertificateRequest]
      req.senderPublicKey should be('defined)
      req.senderPublicKey.get shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.certificate shouldEqual Some(Base64.encode(tx.certificate))
      req.timestamp should be('defined)
      req.timestamp.get shouldEqual tx.timestamp
    }
  }

  property("JSON format validation") {
    val certBase64 = "MIIBGDCBvqADAgECAgYBlyhS/aAwCgYIKoZIzj0EAwIwEzERMA8GA1UEAwwITFRPIFRlc3QwHhcNMjUwNTMxMjE1MDA2WhcNMjYwNTMxMjE1MDE2WjATMREwDwYDVQQDDAhMVE8gVGVzdDBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABFW389kboq3L9JIHM1crkUSmmVsxedvv9fZxi0nwaqbJjLxbvW9bOKCsZutF6r6nyRHCMbnxb1Mx4AgV/ZRqhoYwCgYIKoZIzj0EAwIDSQAwRgIhAP8vRA82EWqQPlxnBDz+DbSZ5AJSIy7lwhTHMMhSmnYeAiEAoX8xafFZodjzuFy8Hli1GbygFH9lsSVQWHw7H5o4QpM="
    val certBytes = Base64.decode(certBase64).get
    val json = Json.parse(s"""
    {
      "type": 24,
      "version": 3,
      "id": "2JWHdujDTBmAvBCr4uWCQioUxQBCwpPJjVAbRYSZL3aU",
      "sender": "3N97aaB866cFf15NZu7xVGGhYNF5YuJaiJG",
      "senderKeyType": "secp256r1",
      "senderPublicKey": "x1Hr4HTHPeGV4oLUtuLtMQP1FqhtrRWpAEaeK6WHjKj9",
      "fee": 100000,
      "timestamp": 1526911531530,
      "certificate": "$certBase64",
      "proofs": [
        "24VXZn6t2ZY1RF8SQiNmeHH8Qiy6P4zRnpkjbvTLcfsucb8zrzec4GRoLtSqkaELZpJXaeoPh83jf14LVgVZeiKZ"
      ]
    }
    """)

    val sender = PublicKeyAccount.fromBase58String(KeyTypes.SECP256R1, "x1Hr4HTHPeGV4oLUtuLtMQP1FqhtrRWpAEaeK6WHjKj9").explicitGet()
    val proof = ByteStr.decodeBase58("24VXZn6t2ZY1RF8SQiNmeHH8Qiy6P4zRnpkjbvTLcfsucb8zrzec4GRoLtSqkaELZpJXaeoPh83jf14LVgVZeiKZ").get

    val tx = CertificateTransaction.create(
      3,
      None,
      1526911531530L,
      sender,
      100000,
      certBytes,
      None,
      Proofs(Seq(proof))
    ).explicitGet()

    tx.json() shouldEqual json
  }
}
