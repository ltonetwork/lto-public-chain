package com.ltonetwork.it.sync.smartcontract

import com.ltonetwork.api.http.requests.smart.SetScriptV1Request
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.transactions.BaseTransactionSuite

class ScriptCompilationSuite extends BaseTransactionSuite {
  test("Sign broadcast via rest") {
    val sender = notMiner.publicKey.address
    notMiner.signAndBroadcast(SetScriptV1Request(1, sender, None, 100000000, None).toJsObject)
  }
}
