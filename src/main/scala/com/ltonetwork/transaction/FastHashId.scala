package com.ltonetwork.transaction

import com.ltonetwork.crypto
import com.ltonetwork.state.ByteStr
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
