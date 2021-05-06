package com.ltonetwork.it.sync.smartcontract

import com.ltonetwork.api.http.assets.SetScriptRequest
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.transactions.BaseTransactionSuite

class ScriptCompilationSuite extends BaseTransactionSuite {
  test("Sign broadcast via rest") {
    val sender = notMiner.publicKey.address
    notMiner.signAndBroadcast(SetScriptRequest(1, sender, None, 100000000, None).toJsObject)
  }
}
