package com.wavesplatform.state.diffs

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.block.Block
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.{Blockchain, ByteStr, EitherExt2}
import com.wavesplatform.transaction.{AssociationTransaction, GenesisTransaction, IssueAssociationTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class AssociationTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

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
      version               <- Gen.oneOf(AssociationTransaction.supportedVersions.toSeq)
      tx = IssueAssociationTransaction
        .selfSigned(version, master, party, 42, None, Constants.UnitsInLTO + feeOverhead, ts + 10000)
        .explicitGet()
    } yield (genesis, tx)

    forAll(setup) {
      case (genesis, assoc) =>
        assertDiffAndState(Seq(block(Seq(genesis))), block(Seq(assoc))) {
          case (d, b) =>
            b.associations(assoc.sender) shouldBe Blockchain.Associations(List((2, assoc)), List.empty)
            b.associations(assoc.assoc.party) shouldBe Blockchain.Associations(List.empty, List((2, assoc)))
        }
    }
  }

  property("multiple incoming assocs") {
    val setup = for {
      master  <- accountGen
      master2 <- accountGen
      party   <- accountGen
      ts      <- positiveLongGen
      version <- Gen.oneOf(AssociationTransaction.supportedVersions.toSeq)
      genesis1 = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2 = GenesisTransaction.create(master2, ENOUGH_AMT, ts).explicitGet()
      feeOverhead <- Gen.choose[Long](0, 100)
      tx1 = IssueAssociationTransaction
        .selfSigned(version,
                    master,
                    party,
                    100,
                    Some(ByteStr.decodeBase58("Fjn9ZkwYx1YuXDskEGDhLA8PdQGgewHRK9PGxYmzy61g").get),
                    Constants.UnitsInLTO + feeOverhead,
                    ts + 1)
        .explicitGet()
      tx2 = IssueAssociationTransaction
        .selfSigned(version, master, party, 11, None, Constants.UnitsInLTO + feeOverhead, ts + 2)
        .explicitGet()
      tx3 = IssueAssociationTransaction
        .selfSigned(version, master2, party, 11, None, Constants.UnitsInLTO + feeOverhead, ts + 3)
        .explicitGet()
    } yield (genesis1, genesis2, tx1, tx2, tx3)

    forAll(setup) {
      case (genesis1, genesis2, tx1, tx2, tx3) =>
        def assert(s: Seq[Block]) = assertDiffAndState(block(Seq(genesis1, genesis2)) +: s.init, s.last) {
          case (d, b) =>
            withClue("for " + s.map(_.transactionData.size)) {
              b.associations(tx1.assoc.party).outgoing shouldBe List.empty
              val incoming = b.associations(tx1.assoc.party).incoming.map(_._2).toSet
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
