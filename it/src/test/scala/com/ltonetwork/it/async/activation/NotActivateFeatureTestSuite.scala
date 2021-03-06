package com.ltonetwork.it.async.activation

import com.typesafe.config.Config
import com.ltonetwork.features.BlockchainFeatureStatus
import com.ltonetwork.features.api.{FeatureActivationStatus, NodeFeatureStatus}
import com.ltonetwork.it.api.AsyncHttpApi._
import com.ltonetwork.it.api.BlockHeaders
import com.ltonetwork.it.transactions.NodesFromDocker
import com.ltonetwork.it.{NodeConfigs, ReportingTestName}
import org.scalatest.{CancelAfterFailure, FreeSpec, Ignore, Matchers}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@Ignore
class NotActivateFeatureTestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ActivationStatusRequest
    with ReportingTestName
    with NodesFromDocker {

  private val votingInterval             = 14
  private val blocksForActivation        = 14
  private val votingFeatureNum: Short    = 1
  private val nonVotingFeatureNum: Short = 4
  private var activationStatusInfoBefore = Option.empty[FeatureActivationStatus]
  private var activationStatusInfoAfter  = Option.empty[FeatureActivationStatus]

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(
        _.raw(
          s"""lto {
         |  blockchain {
         |    custom {
         |      functionality {
         |        pre-activated-features = {}
         |        feature-check-blocks-period = $votingInterval
         |        blocks-for-feature-activation = $blocksForActivation
         |      }
         |    }
         |  }
         |  features.supported=[$nonVotingFeatureNum]
         |  miner.quorum = 3
         |}""".stripMargin
        ))
      .withDefault(4)
      .buildNonConflicting()

  "get activation status info" in {
    activationStatusInfoBefore = Some(activationStatus(nodes, votingInterval - 1, votingFeatureNum, 4.minute))
    activationStatusInfoAfter = Some(activationStatus(nodes, votingInterval + 1, votingFeatureNum, 4.minute))
  }

  "supported blocks is not increased when nobody votes for feature" in {
    val generatedBlocks: Seq[BlockHeaders] = Await.result(nodes.head.blockHeadersSeq(1, votingInterval - 1), 2.minute)
    val featuresMapInGeneratedBlocks       = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1                   = featuresMapInGeneratedBlocks.getOrElse(votingFeatureNum, Seq.empty).length

    votesForFeature1 shouldBe 0
    activationStatusInfoBefore.foreach(assertVotingStatus(_, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented))
  }

  "feature is still in VOTING status on the next voting interval" in {
    activationStatusInfoAfter.foreach(assertVotingStatus(_, 0, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented))
  }

}
