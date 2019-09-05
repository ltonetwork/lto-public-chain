package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.account.PublicKeyAccount._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.AssociationTransaction
import org.scalatest.CancelAfterFailure
import play.api.libs.json._
import com.wavesplatform.it.util._
class AssociationTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee = 1.waves
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
    val assocId = sender
      .signedBroadcast(assocTx.json() + ("type" -> JsNumber(AssociationTransaction.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(assocId)
    val assocs = notMiner.getAssociations(notMiner.address)

    println(assocs)

    assocs.address shouldBe notMiner.address

    assocs.outgoing.size shouldBe 1
    assocs.incoming.size shouldBe 0
    val singleOutgiongAssociation = assocs.outgoing.head
    singleOutgiongAssociation.associationType shouldBe 42
    singleOutgiongAssociation.hash shouldBe ""
    singleOutgiongAssociation.transactionId shouldBe assocId

    val assocs2 = notMiner.getAssociations(party.address)
    assocs2.outgoing.size shouldBe 0
    assocs2.incoming.size shouldBe 1
    val singleIncomingAssociation = assocs2.incoming.head
    singleIncomingAssociation.associationType shouldBe 42
    singleIncomingAssociation.hash shouldBe ""
    singleIncomingAssociation.transactionId shouldBe assocId
  }


}
