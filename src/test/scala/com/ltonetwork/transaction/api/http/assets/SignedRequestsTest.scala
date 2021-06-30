package com.ltonetwork.transaction.api.http.assets

import com.ltonetwork.api.http.requests.TransferRequest
import com.ltonetwork.state._
import com.ltonetwork.utils.Base58
import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json

class SignedRequestsTest extends FunSuite with Matchers {

  test("AssetTransfer json parsing works") {
    val json =
      """
        |{
        |   "recipient":"3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg",
        |   "timestamp":1479462208828,
        |   "amount":100000,
        |   "fee":100000,
        |   "senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |   "signature":"4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ",
        |   "attachment":"A"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[TransferRequest].get
    req.recipient shouldBe "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg"
    req.timestamp should be ('defined)
    req.timestamp.get shouldBe 1479462208828L
    req.amount shouldBe 100000
    req.fee shouldBe 100000
    req.senderPublicKey should be ('defined)
    req.senderPublicKey.get shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    req.signature should be ('defined)
    req.signature.get.base58 shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
    req.attachment should be ('defined)
    req.attachment.get.base58 shouldBe "A"

    val tx = req.toTx.explicitGet()
    Base58.encode(tx.sender.publicKey) shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    tx.timestamp shouldBe 1479462208828L
    tx.attachment shouldBe Base58.decode("A").get
    tx.amount shouldBe 100000
    tx.fee shouldBe 100000
    tx.signature.base58 shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
  }

}
