package com.wavesplatform.transaction.api.http.assets

import com.wavesplatform.state.EitherExt2
import com.wavesplatform.utils.Base58
import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import com.wavesplatform.api.http.assets._

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
    val req = Json.parse(json).validate[SignedTransferV1Request].get
    req.recipient shouldBe "3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg"
    req.timestamp shouldBe 1479462208828L
    req.amount shouldBe 100000
    req.fee shouldBe 100000
    req.senderPublicKey shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    req.signature shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
    req.attachment shouldBe Some("A")

    val tx = req.toTx.explicitGet()
    Base58.encode(tx.sender.publicKey) shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    tx.timestamp shouldBe 1479462208828L
    tx.attachment shouldBe Base58.decode("A").get
    tx.amount shouldBe 100000
    tx.fee shouldBe 100000
    tx.signature.base58 shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
  }

}
