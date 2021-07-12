package com.ltonetwork.transaction.api.http.leasing

import com.ltonetwork.api.http.requests.{CancelLeaseRequest, LeaseRequest}
import com.ltonetwork.state.ByteStr
import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json

class LeaseRequestsTests extends FunSuite with Matchers {

  test("LeaseRequest") {
    val json =
      """
        {
          "amount": 100000,
          "recipient": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 1000
        }
      """

    val req = Json.parse(json).validate[LeaseRequest].get

    req shouldBe LeaseRequest(
      sender = Some("3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb"),
      amount = 100000,
      fee = 1000,
      recipient = "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7"
    )
  }

  test("LeaseCancelRequest") {
    val json =
      """
        {
          "sender": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "txId": "ABMZDPY4MyQz7kKNAevw5P9eNmRErMutJoV9UNeCtqRV",
          "fee": 10000000
        }
      """

    val req = Json.parse(json).validate[CancelLeaseRequest].get

    req shouldBe CancelLeaseRequest(
      sender = Some("3Myss6gmMckKYtka3cKCM563TBJofnxvfD7"),
      leaseId = ByteStr.decodeBase58("ABMZDPY4MyQz7kKNAevw5P9eNmRErMutJoV9UNeCtqRV").get,
      fee = 10000000
    )
  }

  test("Signed LeaseRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "recipient":"3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
         "fee":100000,
         "timestamp":0,
         "amount":1000000,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[LeaseRequest].get

    req shouldBe LeaseRequest(
      senderPublicKey = Some("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw"),
      fee = 100000L,
      amount = 1000000L,
      recipient = "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
      timestamp = Some(0L),
      signature = Some(ByteStr.decodeBase58("4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC").get)
    )
  }

  test("Signed CancelLeaseRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "txId":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
         "timestamp":0,
         "fee": 1000000,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[CancelLeaseRequest].get

    req shouldBe CancelLeaseRequest(
      senderPublicKey = Some("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw"),
      leaseId = ByteStr.decodeBase58("D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5").get,
      timestamp = Some(0L),
      signature = Some(ByteStr.decodeBase58("4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC").get),
      fee = 1000000L
    )
  }
}
