package com.ltonetwork.it.sync.transactions

import com.ltonetwork.account.PublicKeyAccount._
import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.api.http.AssociationsApiRoute.AssociationInfo
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.transactions.BaseTransactionSuite
import com.ltonetwork.it.util._
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.TransactionBuilder
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import org.scalatest.CancelAfterFailure
class AssociationTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee   = 1.lto
  val party = PrivateKeyAccount.fromSeed("party").explicitGet()

  def verifyAssoc(a: AssociationInfo)(party: String, hash: String, tpe: Int, issueTxId: String, revokeTxId: Option[String]) = {
    a.party shouldBe party
    a.associationType shouldBe tpe
    a.hash shouldBe hash
    a.issueTransactionId shouldBe issueTxId
    a.revokeTransactionId shouldBe revokeTxId
  }

  def build(builder: TransactionBuilder, from: PrivateKeyAccount, to: Address, id: Int) = {
    val ts = System.currentTimeMillis()
    (builder match {
      case RevokeAssociationTransaction => RevokeAssociationTransaction.signed(1, ts, from, fee, to, id, None)
      case IssueAssociationTransaction  => IssueAssociationTransaction.signed(1, ts, from, fee, to, id, None, None)
    }).explicitGet()
  }
  def postAssoc(assocTx: AssociationTransaction) = {
    val assocId = sender
      .signedBroadcast(assocTx.json())
      .id
    nodes.waitForHeightAriseAndTxPresent(assocId)
    assocId
  }
  def assertError(assocTx: AssociationTransaction, err: String) = {
    assertBadRequestAndResponse(sender.signedBroadcast(assocTx.json()), err)
  }

  test("post and revoke association") {
    val assocId = postAssoc(build(IssueAssociationTransaction, notMiner.privateKey, party.toAddress, 42))

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

    val revokeId = postAssoc(build(RevokeAssociationTransaction, notMiner.privateKey, party.toAddress, 42))

    val revokedAliceAssocs = notMiner.getAssociations(alice)
    revokedAliceAssocs.outgoing.size shouldBe 1
    revokedAliceAssocs.incoming.size shouldBe 0
    verifyAssoc(revokedAliceAssocs.outgoing.head)(bob, "", 42, assocId, Some(revokeId))

    val revokedBobAssocs = notMiner.getAssociations(bob)
    revokedBobAssocs.outgoing.size shouldBe 0
    revokedBobAssocs.incoming.size shouldBe 1
    verifyAssoc(revokedBobAssocs.incoming.head)(alice, "", 42, assocId, Some(revokeId))
  }

  test("can't revoke non-existing assoc") {
    postAssoc(build(IssueAssociationTransaction, notMiner.privateKey, party.toAddress, 88))

    assertError(build(IssueAssociationTransaction, notMiner.privateKey, party.toAddress, 88), ".+already.+")

    assertError(build(RevokeAssociationTransaction, notMiner.privateKey, party.toAddress, 89), ".+doesn't exist.+")

    postAssoc(build(IssueAssociationTransaction, notMiner.privateKey, party.toAddress, 89))
    postAssoc(build(RevokeAssociationTransaction, notMiner.privateKey, party.toAddress, 89))
  }
}
