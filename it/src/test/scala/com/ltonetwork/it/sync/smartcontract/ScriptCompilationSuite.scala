package com.ltonetwork.it.sync.smartcontract

import com.ltonetwork.api.http.requests.SetScriptRequest
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.transactions.BaseTransactionSuite
import play.api.libs.json.{Json, OWrites}

class ScriptCompilationSuite extends BaseTransactionSuite {
  test("Sign broadcast via rest") {
    val sender = notMiner.publicKey.address
    val request = SetScriptRequest(
      version = Some(1),
      sender = Some(sender),
      fee = 100000000,
      script = None
    )
    notMiner.signAndBroadcast(Json.toJsObject(request))
  }
}
