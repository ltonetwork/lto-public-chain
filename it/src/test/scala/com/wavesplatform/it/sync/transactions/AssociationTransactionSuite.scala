package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.account.PublicKeyAccount._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.AssociationTransaction
import org.scalatest.CancelAfterFailure
import play.api.libs.json._

class AssociationTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee = 10^8
  test("post association") {

    val party = PrivateKeyAccount.fromSeed("party").explicitGet()

    val assocTx = AssociationTransaction.selfSigned(
      version = 1,
      sender = notMiner.privateKey,
      party = party.toAddress,
      assocType = 42,
      hash = None,
      feeAmount = fee,
      timestamp = System.currentTimeMillis()).explicitGet()
    val transferId = sender
      .signedBroadcast(assocTx.json() + ("type" -> JsNumber(AssociationTransaction.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
    val assocs = notMiner.getAssociations(notMiner.address)
    assocs.address shouldBe notMiner.address
    val singleOutgiongAssociation = assocs.outgoing(0)
    singleOutgiongAssociation.associationType shouldBe 42
    singleOutgiongAssociation.hash shouldBe ""
    singleOutgiongAssociation.transactionId shouldBe transferId
  }


}
