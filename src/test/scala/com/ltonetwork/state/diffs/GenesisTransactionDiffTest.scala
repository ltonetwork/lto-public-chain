package com.ltonetwork.state.diffs

import cats._
import com.ltonetwork.block.TestBlock
import com.ltonetwork.state._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class GenesisTransactionDiffTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {
  def nelMax[T](g: Gen[T], max: Int = 10): Gen[List[T]] = Gen.choose(1, max).flatMap(Gen.listOfN(_, g))

  property("fails if height != 1") {
    forAll(genesisGen, positiveIntGen suchThat (_ > 1)) { (gtx, h) =>
      GenesisTransactionDiff(h)(gtx) should produce("GenesisTransaction cannot appear in non-initial block")
    }
  }

  property("Diff establishes LTO invariant") {
    forAll(nelMax(genesisGen)) { gtxs =>
      assertDiffAndState(Seq.empty, TestBlock.create(gtxs)) { (blockDiff, _) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.effectiveBalance shouldBe gtxs.map(_.amount).sum

        gtxs.foreach { gtx =>
          blockDiff.portfolios(gtx.recipient).balance shouldBe gtx.amount
        }
      }
    }
  }
}
