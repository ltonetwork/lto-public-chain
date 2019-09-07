package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.account.PublicKeyAccount._
import com.wavesplatform.api.http.AddressApiRoute.AssociationInfo
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.AssociationTransaction
import org.scalatest.CancelAfterFailure
import play.api.libs.json._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.AssociationTransaction.ActionType
class AssociationTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee = 1.waves
  val party = PrivateKeyAccount.fromSeed("party").explicitGet()

  def verifyAssoc(a: AssociationInfo)(party:String,hash: String,tpe:Int,issueTxId:String,revokeTxId:Option[String]) = {
    a.party shouldBe party
    a.associationType shouldBe tpe
    a.hash shouldBe hash
    a.issueTransactionId shouldBe issueTxId
    a.revokeTransactionId shouldBe revokeTxId
  }
   def postAssoc(tpe: ActionType) = {
     val assocTx = AssociationTransaction.selfSigned(
       version = 1,
       sender = notMiner.privateKey,
       party = party.toAddress,
       assocType = 42,
       hash = None,
       action = tpe,
       feeAmount = fee,
       timestamp = System.currentTimeMillis()).explicitGet()
     val assocId = sender
       .signedBroadcast(assocTx.json() + ("type" -> JsNumber(AssociationTransaction.typeId.toInt)))
       .id
     nodes.waitForHeightAriseAndTxPresent(assocId)
     assocId

   }


  test("post and revoke association") {
    val assocId = postAssoc(AssociationTransaction.ActionType.Issue)

    val assocs = notMiner.getAssociations(notMiner.address)
    assocs.address shouldBe notMiner.address
    assocs.outgoing.size shouldBe 1
    assocs.incoming.size shouldBe 0
    val singleOutgiongAssociation = assocs.outgoing.head
    verifyAssoc(singleOutgiongAssociation)(party.address,"",42,assocId,None)

    val assocs2 = notMiner.getAssociations(party.address)
    assocs2.outgoing.size shouldBe 0
    assocs2.incoming.size shouldBe 1
    val singleIncomingAssociation = assocs2.incoming.head
    verifyAssoc(singleIncomingAssociation)(notMiner.address,"",42,assocId,None)

    val revokeId = postAssoc(AssociationTransaction.ActionType.Revoke)


    val revokedAssocs = notMiner.getAssociations(notMiner.address)
    revokedAssocs.address shouldBe notMiner.address
    revokedAssocs.outgoing.size shouldBe 1
    revokedAssocs.incoming.size shouldBe 0
    val singleOutgiongRevokedAssociation = assocs.outgoing.head
    verifyAssoc(singleOutgiongRevokedAssociation)(party.address,"",42,assocId,Some(revokeId))

    val revokedAssocs2 = notMiner.getAssociations(party.address)
    revokedAssocs2.outgoing.size shouldBe 0
    revokedAssocs2.incoming.size shouldBe 1
    val singleIncomingRevokedAssociation = revokedAssocs2.incoming.head
    verifyAssoc(singleIncomingRevokedAssociation)(notMiner.address,"",42,assocId,Some(revokeId))
  }

}
