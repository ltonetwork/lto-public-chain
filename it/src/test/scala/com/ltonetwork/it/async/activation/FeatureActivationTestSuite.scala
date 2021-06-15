package com.ltonetwork.it.async.activation

import com.typesafe.config.Config
import com.ltonetwork.features.api.NodeFeatureStatus
import com.ltonetwork.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.ltonetwork.it.transactions.NodesFromDocker
import com.ltonetwork.it.{NodeConfigs, ReportingTestName}
import org.scalatest.{CancelAfterFailure, FreeSpec, Ignore, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@Ignore
class FeatureActivationTestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName {

  private val waitCompletion = 6.minutes

  private val votingInterval      = 12
  private val blocksForActivation = 12 // should be even
  private val featureNum: Short   = BlockchainFeatures.SmallerMinimalGeneratingBalance.id
  private val featureDescr        = BlockchainFeatures.UnknownFeature

  override protected def nodeConfigs: Seq[Config] = {
    NodeConfigs.newBuilder
      .overrideBase(_.raw(s"""lto {
                               |  blockchain.custom.functionality {
                               |    pre-activated-features = null
                               |    feature-check-blocks-period = $votingInterval
                               |    blocks-for-feature-activation = $blocksForActivation
                               |  }
                               |  features.supported = [$featureNum]
                               |  miner.quorum = 3
                               |}""".stripMargin))
      .withDefault(4)
      .buildNonConflicting()
  }

  "supported blocks increased when voting starts" in {
    val checkHeight = votingInterval * 2 / 3
    val status      = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    status.description shouldBe featureDescr
    assertVotingStatus(status, status.supportingBlocks.get, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
  }

  "supported blocks counter resets on the next voting interval" in {
    val checkHeight = votingInterval * 2 - blocksForActivation / 2
    val info        = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    info.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
  }

  "blockchain status is APPROVED in second voting interval" in {
    val checkHeight = votingInterval * 2
    val statusInfo  = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    statusInfo.description shouldBe featureDescr
    // Activation will be on a next voting interval
    assertApprovedStatus(statusInfo, checkHeight + votingInterval, NodeFeatureStatus.Voted)
  }

  "blockchain status is ACTIVATED in third voting interval" in {
    val checkHeight = votingInterval * 3
    val statusInfo  = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    statusInfo.description shouldBe featureDescr
    assertActivatedStatus(statusInfo, checkHeight, NodeFeatureStatus.Voted)
  }
}
