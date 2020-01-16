package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.account.PublicKeyAccount._
import com.wavesplatform.api.http.AddressApiRoute.AssociationInfo
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import org.scalatest.CancelAfterFailure
import play.api.libs.json._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.AssociationTransaction.ActionType
class AssociationTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee   = 1.waves
  val party = PrivateKeyAccount.fromSeed("party").explicitGet()

  def verifyAssoc(a: AssociationInfo)(party: String, hash: String, tpe: Int, issueTxId: String, revokeTxId: Option[String]) = {
    a.party shouldBe party
    a.associationType shouldBe tpe
    a.hash shouldBe hash
    a.issueTransactionId shouldBe issueTxId
    a.revokeTransactionId shouldBe revokeTxId
  }

  test("post and revoke association") {
    def postAssoc(tpe: ActionType) = {
      val m = tpe match {
        case ActionType.Revoke => RevokeAssociationTransaction.selfSigned _
        case ActionType.Issue => IssueAssociationTransaction.selfSigned _
      }
      val assocTx = m(1, notMiner.privateKey, party.toAddress, 42, None, fee, System.currentTimeMillis()).explicitGet()
      val assocId = sender
        .signedBroadcast(assocTx.json())
        .id
      nodes.waitForHeightAriseAndTxPresent(assocId)
      assocId

    }

    val assocId = postAssoc(AssociationTransaction.ActionType.Issue)

    val alice = notMiner.address
    val bob   = party.address

    val aliceAssocs = notMiner.getAssociations(alice)
    aliceAssocs.address shouldBe alice
    aliceAssocs.outgoing.size shouldBe 1
    aliceAssocs.incoming.size shouldBe 0
    verifyAssoc(aliceAssocs.outgoing.head)(bob, "", 42, assocId, None)

    val bobAssocs = notMiner.getAssociations(bob)
    bobAssocs.outgoing.size shouldBe 0
    bobAssocs.incoming.size shouldBe 1
    verifyAssoc(bobAssocs.incoming.head)(alice, "", 42, assocId, None)

    val revokeId = postAssoc(AssociationTransaction.ActionType.Revoke)

    val revokedAliceAssocs = notMiner.getAssociations(alice)
    revokedAliceAssocs.outgoing.size shouldBe 1
    revokedAliceAssocs.incoming.size shouldBe 0
    verifyAssoc(revokedAliceAssocs.outgoing.head)(bob, "", 42, assocId, Some(revokeId))

    val revokedBobAssocs = notMiner.getAssociations(bob)
    revokedBobAssocs.outgoing.size shouldBe 0
    revokedBobAssocs.incoming.size shouldBe 1
    verifyAssoc(revokedBobAssocs.incoming.head)(alice, "", 42, assocId, Some(revokeId))
  }

}
