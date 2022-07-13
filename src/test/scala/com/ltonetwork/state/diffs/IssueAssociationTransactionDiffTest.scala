package com.ltonetwork.state.diffs

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.block.Block
import com.ltonetwork.block.TestBlock.{create => block}
import com.ltonetwork.settings.Constants
import com.ltonetwork.state.{Associations, ByteStr, EitherExt2}
import com.ltonetwork.transaction.association.IssueAssociationTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class IssueAssociationTransactionDiffTest
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDB {

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  property("can find outgoing and incoming assoc") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      recipient             <- accountGen
      feeOverhead           <- Gen.choose[Long](0, ENOUGH_AMT)
      version               <- Gen.oneOf(IssueAssociationTransaction.supportedVersions.toSeq)
      tx = IssueAssociationTransaction
        .signed(version, ts + 10000, master, Constants.UnitsInLTO + feeOverhead, 42, recipient, None, None, List.empty)
        .explicitGet()
    } yield (genesis, tx)

    forAll(setup) {
      case (genesis, assoc) =>
        assertDiffAndState(Seq(block(Seq(genesis))), block(Seq(assoc))) {
          case (d, b) =>
            val timestamp = b.lastBlockTimestamp.get
            b.associations(assoc.sender) shouldBe Associations.build(assoc.sender, timestamp + 1, List((timestamp, assoc)), List.empty)
            b.associations(assoc.recipient) shouldBe Associations.build(assoc.recipient, timestamp + 1, List.empty, List((timestamp, assoc)))
        }
    }
  }

  property("multiple incoming assocs") {
    val setup = for {
      master  <- accountGen
      master2 <- accountGen
      recipient   <- accountGen
      ts      <- positiveLongGen
      version <- Gen.oneOf(IssueAssociationTransaction.supportedVersions.toSeq)
      genesis1 = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2 = GenesisTransaction.create(master2, ENOUGH_AMT, ts).explicitGet()
      feeOverhead <- Gen.choose[Long](0, 100)
      tx1 = IssueAssociationTransaction
        .signed(
          version,
          ts + 1,
          master,
          Constants.UnitsInLTO + feeOverhead,
          100,
          recipient,
          None,
          Some(ByteStr.decodeBase58("Fjn9ZkwYx1YuXDskEGDhLA8PdQGgewHRK9PGxYmzy61g").get),
          List.empty,
        )
        .explicitGet()
      tx2 = IssueAssociationTransaction
        .signed(version, ts + 2, master, Constants.UnitsInLTO + feeOverhead, 11, recipient, None, None, List.empty)
        .explicitGet()
      tx3 = IssueAssociationTransaction
        .signed(version, ts + 3, master2, Constants.UnitsInLTO + feeOverhead, 11, recipient, None, None, List.empty)
        .explicitGet()
    } yield (genesis1, genesis2, tx1, tx2, tx3)

    forAll(setup) {
      case (genesis1, genesis2, tx1, tx2, tx3) =>
        def assert(s: Seq[Block]): Unit = assertDiffAndState(block(Seq(genesis1, genesis2)) +: s.init, s.last) {
          case (d, b) =>
            withClue("for " + s.map(_.transactionData.size)) {
              // Last block is turned into a diff, so in that case use blockchain timestamp instead
              val timestamp = b.lastBlockTimestamp.get
              val ts1 = s.init.find(_.transactionData.contains(tx1)).fold(timestamp)(_.timestamp)
              val ts2 = s.init.find(_.transactionData.contains(tx2)).fold(timestamp)(_.timestamp)
              val ts3 = s.init.find(_.transactionData.contains(tx3)).fold(timestamp)(_.timestamp)

              b.associations(tx1.recipient).outgoing shouldBe List.empty
              val associationsRecipient = Associations.build(tx1.recipient, timestamp + 1, List((ts1, tx1), (ts2, tx2), (ts3, tx3)))
              b.associations(tx1.recipient).incoming shouldBe associationsRecipient.incoming

              val associationsMaster1 = Associations.build(tx1.sender, timestamp + 1, List((ts1, tx1), (ts2, tx2)))
              b.associations(tx1.sender).outgoing shouldBe associationsMaster1.outgoing
              b.associations(tx1.sender).incoming shouldBe List.empty

              val associationsMaster2 = Associations.build(tx3.sender, timestamp + 1, List((ts3, tx3)))
              b.associations(tx3.sender).outgoing shouldBe associationsMaster2.outgoing
              b.associations(tx3.sender).incoming shouldBe List.empty
            }
        }
        assert(Seq(block(Seq(tx1, tx2, tx3))))
        assert(Seq(block(Seq(tx1)), block(Seq(tx2, tx3))))
        assert(Seq(block(Seq(tx1, tx2)), block(Seq(tx3))))
    }
  }

}
