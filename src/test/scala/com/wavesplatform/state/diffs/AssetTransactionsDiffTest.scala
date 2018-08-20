package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.utils.dummyCompilerContext
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import fastparse.core.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.transfer._

class AssetTransactionsDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  def issueReissueBurnTxs(isReissuable: Boolean): Gen[((GenesisTransaction, IssueTransaction), (ReissueTransaction, BurnTransaction))] =
    for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      ia                     <- positiveLongGen
      ra                     <- positiveLongGen
      ba                     <- positiveLongGen.suchThat(x => x < ia + ra)
      (issue, reissue, burn) <- issueReissueBurnGeneratorP(ia, ra, ba, master) suchThat (_._1.reissuable == isReissuable)
    } yield ((genesis, issue), (reissue, burn))

  property("Issue+Reissue+Burn do not break waves invariant and updates state") {
    forAll(issueReissueBurnTxs(isReissuable = true)) {
      case (((gen, issue), (reissue, burn))) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue, burn))) {
          case (blockDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)

            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0

            val totalAssetVolume = issue.quantity + reissue.quantity - burn.quantity
        }
    }
  }

  property("Cannot reissue/burn non-existing alias") {
    val setup: Gen[(GenesisTransaction, ReissueTransaction, BurnTransaction)] = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      reissue <- reissueGen
      burn    <- burnGen
    } yield (genesis, reissue, burn)

    forAll(setup) {
      case ((gen, reissue, burn)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
        assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(burn))) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
    }
  }

  property("Cannot reissue/burn non-owned alias") {
    val setup = for {
      ((gen, issue), (_, _)) <- issueReissueBurnTxs(isReissuable = true)
      other                  <- accountGen.suchThat(_ != issue.sender.toAddress)
      quantity               <- positiveLongGen
      reissuable2            <- Arbitrary.arbitrary[Boolean]
      fee                    <- Gen.choose(1L, 2000000L)
      timestamp              <- timestampGen
      reissue = ReissueTransactionV1.selfSigned(other, issue.assetId(), quantity, reissuable2, fee, timestamp).explicitGet()
      burn    = BurnTransactionV1.selfSigned(other, issue.assetId(), quantity, fee, timestamp).explicitGet()
    } yield ((gen, issue), reissue, burn)

    forAll(setup) {
      case ((gen, issue), reissue, burn) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(burn))) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
    }
  }

  property("Can not reissue > long.max") {
    val setup = for {
      issuer    <- accountGen
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer, ENOUGH_AMT, timestamp).explicitGet()
      assetName   <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      decimals    <- Gen.choose(1: Byte, 8: Byte)
      issue       <- createIssue(issuer, assetName, description, quantity, decimals, true, fee, timestamp)
      assetId = issue.assetId()
      reissue = ReissueTransactionV1.selfSigned(issuer, assetId, Long.MaxValue, true, 1, timestamp).explicitGet()
    } yield (issuer, assetId, genesis, issue, reissue)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.DataTransaction.id -> 0)
        )

    forAll(setup) {
      case (issuer, assetId, genesis, issue, reissue) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(reissue)), fs) { ei =>
          ei should produce("Asset total value overflow")
        }
    }
  }

  property("Can not total issue > long.max") {
    val setup = for {
      issuer    <- accountGen
      holder    <- accountGen.suchThat(_ != issuer)
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer, ENOUGH_AMT, timestamp).explicitGet()
      assetName   <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      decimals    <- Gen.choose(1: Byte, 8: Byte)
      issue       <- createIssue(issuer, assetName, description, quantity, decimals, true, fee, timestamp)
      assetId = issue.assetId()
      attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
      transfer = TransferTransactionV1.selfSigned(issuer, holder, quantity - 1, timestamp, fee, attachment).explicitGet()
      reissue  = ReissueTransactionV1.selfSigned(issuer, assetId, (Long.MaxValue - quantity) + 1, true, 1, timestamp).explicitGet()
    } yield (issuer, assetId, genesis, issue, reissue, transfer)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.DataTransaction.id -> 0)
        )

    forAll(setup) {
      case (issuer, assetId, genesis, issue, reissue, transfer) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue, transfer))), TestBlock.create(Seq(reissue)), fs) { ei =>
          ei should produce("Asset total value overflow")
        }
    }
  }

  property("Cannot reissue non-reissuable alias") {
    forAll(issueReissueBurnTxs(isReissuable = false)) {
      case ((gen, issue), (reissue, _)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Asset is not reissuable")
        }
    }
  }

  private def createScript(code: String) = {
    val Parsed.Success(expr, _) = Parser(code).get
    assert(expr.size == 1)
    ScriptV1(CompilerV1(dummyCompilerContext, expr.head).explicitGet()._1).explicitGet()
  }

  def genesisIssueTransferReissue(code: String): Gen[(Seq[GenesisTransaction], IssueTransactionV2, TransferTransactionV1, ReissueTransactionV1)] =
    for {
      version            <- Gen.oneOf(IssueTransactionV2.supportedVersions.toSeq)
      timestamp          <- timestampGen
      initialWavesAmount <- Gen.choose(Long.MaxValue / 1000, Long.MaxValue / 100)
      accountA           <- accountGen
      accountB           <- accountGen
      smallFee           <- Gen.choose(1l, 10l)
      genesisTx1 = GenesisTransaction.create(accountA, initialWavesAmount, timestamp).explicitGet()
      genesisTx2 = GenesisTransaction.create(accountB, initialWavesAmount, timestamp).explicitGet()
      reissuable = true
      (_, assetName, description, quantity, decimals, _, _, _) <- issueParamGen
      issue = IssueTransactionV2
        .selfSigned(version,
                    AddressScheme.current.chainId,
                    accountA,
                    assetName,
                    description,
                    quantity,
                    decimals,
                    reissuable,
                    Some(createScript(code)),
                    smallFee,
                    timestamp + 1)
        .explicitGet()
      assetId = issue.id()
      transfer = TransferTransactionV1
        .selfSigned(accountA, accountB, issue.quantity, timestamp + 2, smallFee, Array.empty)
        .explicitGet()
      reissue = ReissueTransactionV1.selfSigned(accountB, assetId, quantity, reissuable, smallFee, timestamp + 3).explicitGet()
    } yield (Seq(genesisTx1, genesisTx2), issue, transfer, reissue)

  property("Can issue smart asset with script") {
    forAll(for {
      acc        <- accountGen
      genesis    <- genesisGeneratorP(acc)
      smartIssue <- smartIssueTransactionGen(acc)
    } yield (genesis, smartIssue)) {
      case (gen, issue) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(issue)), smartEnabledFS) {
          case (blockDiff, newState) =>
            newState.assetDescription(issue.id()) shouldBe Some(
              AssetDescription(issue.sender,
                               issue.name,
                               issue.description,
                               issue.decimals,
                               issue.reissuable,
                               BigInt(issue.quantity),
                               issue.script,
                               0L))
            blockDiff.transactions.get(issue.id()).isDefined shouldBe true
            newState.transactionInfo(issue.id()).isDefined shouldBe true
            newState.transactionInfo(issue.id()).isDefined shouldEqual true
        }
    }
  }

}
