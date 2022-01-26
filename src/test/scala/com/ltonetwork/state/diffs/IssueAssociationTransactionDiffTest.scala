package com.ltonetwork.state.diffs

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.block.Block
import com.ltonetwork.block.TestBlock.{create => block}
import com.ltonetwork.settings.Constants
import com.ltonetwork.state.{Blockchain, ByteStr, EitherExt2}
import com.ltonetwork.transaction.association.IssueAssociationTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

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
      party                 <- accountGen
      feeOverhead           <- Gen.choose[Long](0, ENOUGH_AMT)
      version               <- Gen.oneOf(IssueAssociationTransaction.supportedVersions.toSeq)
      tx = IssueAssociationTransaction
        .signed(version, ts + 10000, master, Constants.UnitsInLTO + feeOverhead, party, 42, None, None)
        .explicitGet()
    } yield (genesis, tx)

    forAll(setup) {
      case (genesis, assoc) =>
        assertDiffAndState(Seq(block(Seq(genesis))), block(Seq(assoc))) {
          case (d, b) =>
            b.associations(assoc.sender) shouldBe Blockchain.Associations(List((2, assoc)), List.empty)
            b.associations(assoc.recipient) shouldBe Blockchain.Associations(List.empty, List((2, assoc)))
        }
    }
  }

  property("multiple incoming assocs") {
    val setup = for {
      master  <- accountGen
      master2 <- accountGen
      party   <- accountGen
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
          party,
          100,
          None,
          Some(ByteStr.decodeBase58("Fjn9ZkwYx1YuXDskEGDhLA8PdQGgewHRK9PGxYmzy61g").get)
        )
        .explicitGet()
      tx2 = IssueAssociationTransaction
        .signed(version, ts + 2, master, Constants.UnitsInLTO + feeOverhead, party, 11, None, None)
        .explicitGet()
      tx3 = IssueAssociationTransaction
        .signed(version, ts + 3, master2, Constants.UnitsInLTO + feeOverhead, party, 11, None, None)
        .explicitGet()
    } yield (genesis1, genesis2, tx1, tx2, tx3)

    forAll(setup) {
      case (genesis1, genesis2, tx1, tx2, tx3) =>
        def assert(s: Seq[Block]) = assertDiffAndState(block(Seq(genesis1, genesis2)) +: s.init, s.last) {
          case (d, b) =>
            withClue("for " + s.map(_.transactionData.size)) {
              b.associations(tx1.recipient).outgoing shouldBe List.empty
              val incoming = b.associations(tx1.recipient).incoming.map(_._2).toSet
              incoming shouldBe Set(tx1, tx2, tx3)

              val outgoingMaster1 = b.associations(tx1.sender).outgoing.map(_._2).toSet
              outgoingMaster1 shouldBe Set(tx1, tx2)
              b.associations(tx1.sender).incoming shouldBe List.empty

              val outgoingMaster2 = b.associations(tx3.sender).outgoing.map(_._2).toSet
              outgoingMaster2 shouldBe Set(tx3)
              b.associations(tx3.sender).incoming shouldBe List.empty
            }
        }
        assert(Seq(block(Seq(tx1, tx2, tx3))))
        assert(Seq(block(Seq(tx1)), block(Seq(tx2, tx3))))
        assert(Seq(block(Seq(tx1, tx2)), block(Seq(tx3))))
    }
  }

}
